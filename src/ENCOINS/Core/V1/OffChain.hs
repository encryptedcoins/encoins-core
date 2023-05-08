{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module ENCOINS.Core.V1.OffChain where

import           Control.Monad.State                      (gets, when)
import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Bool                                (bool)
import           Data.Functor                             (($>), (<$>))
import           Data.Text                                (pack)
import           GHC.Generics                             (Generic)
import           Ledger                                   (DecoratedTxOut (..), _decoratedTxOutAddress, noAdaValue)
import           Ledger.Ada                               (lovelaceValueOf)
import           Ledger.Value                             (adaOnlyValue, geq, gt, flattenValue)
import           Plutus.V2.Ledger.Api                     hiding (singleton)
import           PlutusTx.Prelude                         hiding (mapM, (<$>), (<>))
import           Prelude                                  (show, (<>))
import qualified Prelude                                  as Haskell
import           Text.Hex                                 (encodeHex)

import           ENCOINS.BaseTypes                        (MintingPolarity (..))
import           ENCOINS.Bulletproofs                     (polarityToInteger)
import           ENCOINS.Core.V1.OnChain
import           PlutusAppsExtra.Constraints.OffChain
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Scripts.OneShotCurrency  (oneShotCurrencyMintTx)
import           PlutusAppsExtra.Types.Tx                 (TransactionBuilder, TxConstructor (..))
import           PlutusAppsExtra.Utils.Datum              (hashedUnit, inlinedUnit)
import           PlutusAppsExtra.Utils.Value              (unflattenValue, isCurrencyAndAdaOnlyValue)

data EncoinsMode = WalletMode | LedgerMode
    deriving (Haskell.Show, Haskell.Read, Haskell.Eq, Generic, FromJSON, ToJSON)

instance Eq EncoinsMode where
    (==) = (Haskell.==)

protocolFee :: Integer -> EncoinsMode -> Value
protocolFee n mode
    | n < 0 || mode == LedgerMode = lovelaceValueOf $ max f $ (negate n * 1_000_000) `divide` 200
    | otherwise                   = zero
    where f = case mode of
            WalletMode -> 1_500_000
            LedgerMode -> 2_000_000

---------------------------- Stake Owner Token Minting Policy --------------------------------------

stakeOwnerMintTx :: EncoinsProtocolParams -> TransactionBuilder ()
stakeOwnerMintTx par = oneShotCurrencyMintTx (stakeOwnerMintParams par) $> ()

stakeOwnerTx :: EncoinsProtocolParams -> TransactionBuilder ()
stakeOwnerTx = stakeOwnerMintTx

------------------------------------- Beacon Minting Policy ----------------------------------------

beaconMintTx :: EncoinsProtocolParams -> TransactionBuilder ()
beaconMintTx par = oneShotCurrencyMintTx (beaconMintParams par) $> ()

beaconSendTx :: EncoinsProtocolParams -> TransactionBuilder ()
beaconSendTx par = utxoProducedTx
    (ledgerValidatorAddress par)
    (beaconToken par + lovelaceValueOf 2_000_000)
    (Just hashedUnit)

beaconTx :: EncoinsProtocolParams -> TransactionBuilder ()
beaconTx par = do
    beaconMintTx par
    beaconSendTx par

---------------------------------------- Posting Scripts ------------------------------------------

postEncoinsPolicyTx :: EncoinsProtocolParams -> Integer -> TransactionBuilder ()
postEncoinsPolicyTx par salt = postMintingPolicyTx (alwaysFalseValidatorAddress salt) (encoinsPolicyV par) (Just inlinedUnit) zero

postLedgerValidatorTx :: EncoinsProtocolParams -> Integer -> TransactionBuilder ()
postLedgerValidatorTx par salt = postValidatorTx (alwaysFalseValidatorAddress salt) (ledgerValidatorV par) (Just inlinedUnit) zero

--------------------------------------- UTXO Spend and Produce --------------------------------------------

walletSpendTx' :: Value -> TransactionBuilder (Maybe Value)
walletSpendTx' val =
    fmap (_decoratedTxOutValue . snd) <$> utxoSpentPublicKeyTx'
        (\_ o -> _decoratedTxOutValue o `geq` val)

walletSpendTx :: Value -> TransactionBuilder (Maybe Value)
walletSpendTx val = do
    res <- walletSpendTx' val
    if isNothing res
        then do
            utxos <- gets txConstructorLookups
            failTx "walletSpendTx" ("Cannot find a suitable utxo to spend. UTXOs: " <> pack (show utxos)) res
        else return res

-- Spend utxo greater than the given value from the ENCOINS Ledger script.
ledgerSpendTx' :: EncoinsProtocolParams -> Value -> TransactionBuilder (Maybe Value)
ledgerSpendTx' par val =
    fmap (_decoratedTxOutValue . snd) <$> utxoSpentScriptTx'
        (\_ o -> _decoratedTxOutValue o `geq` val
        && isCurrencyAndAdaOnlyValue (encoinsSymbol par) (_decoratedTxOutValue o)
        && _decoratedTxOutAddress o == ledgerValidatorAddress par)
        (const . const $ ledgerValidatorV par) (const . const $ ())

-- Spend utxo greater than the given value from the ENCOINS Ledger script. Fails if the utxo is not found.
ledgerSpendTx :: EncoinsProtocolParams -> Value -> TransactionBuilder (Maybe Value)
ledgerSpendTx par val = do
    res <- ledgerSpendTx' par val
    if isNothing res
        then do
            utxos <- gets txConstructorLookups
            failTx "ledgerSpendTx" ("Cannot find a suitable utxo to spend. UTXOs: " <> pack (show utxos)) res
        else return res

ledgerProduceTx :: EncoinsProtocolParams -> Value -> TransactionBuilder Value
ledgerProduceTx par val =
    let noAdaVal = noAdaValue val
        valChunk = unflattenValue $ take 5 $ flattenValue noAdaVal
    in if noAdaVal == valChunk
        then utxoProducedTx (ledgerValidatorAddress par) val (Just inlinedUnit) $> zero
        else do
            let val' = valChunk + lovelaceValueOf minAdaTxOutInLedger
            utxoProducedTx (ledgerValidatorAddress par) val' (Just inlinedUnit)
            ledgerProduceTx par (val - val')

------------------------------------- ENCOINS Smart Contract -----------------------------------------

-- Returns value spent from the ENCOINS Ledger.
encoinsBurnTx :: EncoinsProtocolParams -> [BuiltinByteString] -> EncoinsMode -> TransactionBuilder Value
encoinsBurnTx _   []  _    = return zero
encoinsBurnTx par bss mode = do
    let bs  = head bss
        val = encoin par bs
    res <- case mode of
      WalletMode -> walletSpendTx val
      LedgerMode -> ledgerSpendTx par val
    case res of
      Nothing -> failTx "encoinsBurnTx" ("Cannot find the required coin: " <> encodeHex (fromBuiltin bs)) Nothing $> zero
      Just v  ->
        -- Filter out all encoins in the output
        let bss' = filter (`notElem` encoinsInValue par v) bss
        -- Sum the current and the future values spent from the ENCOINS Ledger
        in (+) (bool zero v (mode == LedgerMode)) <$> encoinsBurnTx par bss' mode

-- Modify the value locked in the ENCOINS Ledger script by the given value
ledgerModifyTx :: EncoinsProtocolParams -> Value -> TransactionBuilder ()
ledgerModifyTx par val
    | adaOnlyValue val `geq` lovelaceValueOf minAdaTxOutInLedger = ledgerProduceTx par val $> ()
    | otherwise     = do
        let valAda = lovelaceValueOf minAdaTxOutInLedger - adaOnlyValue val
        -- TODO: Spend several utxo to get the required value
        -- TODO: Randomize the selection process
        val'  <- fromMaybe zero <$> ledgerSpendTx par valAda
        -- Spend an additional utxo if possible
        val'' <- fromMaybe zero <$> ledgerSpendTx' par zero
        if val' `gt` zero
            then ledgerModifyTx par (val + val' + val'')
            else failTx "ledgerModifyTx" ("Cannot modify the value in the ENCOINS Ledger: " <> pack (show val)) Nothing $> ()

encoinsTx :: (Address, Address) -> EncoinsProtocolParams -> EncoinsRedeemerOnChain -> EncoinsMode -> TransactionBuilder ()
encoinsTx (addrRelay, addrTreasury) par red@((ledgerAddr, changeAddr), (v, inputs), _, _) mode = do
    -- Checking that the ENCOINS Ledger address is correct
    when (ledgerAddr /= ledgerValidatorAddress par)
        $ failTx "encoinsTx" "ENCOINS Ledger address in the redeemer is not correct" Nothing $> ()

    -- Spending utxos with encoins to burn
    let encoinsToBurn = filter (\(_, p) -> p == Burn) inputs
    valFromLedger <- encoinsBurnTx par (map fst encoinsToBurn) mode

    -- Referencing beacon utxo
    let beacon      = beaconToken par
    utxoReferencedTx (\_ o -> _decoratedTxOutAddress o == ledgerAddr && _decoratedTxOutValue o `geq` beacon) $> ()

    -- Minting and burning encoins
    let valMint  = sum $ map (\(bs, p) -> scale (polarityToInteger p) (encoin par bs)) inputs
    tokensMintedTx (encoinsPolicyV par) red valMint

    -- Modify ENCOINS Ledger by the given value
    let valWithdraw = negate $ lovelaceValueOf (v * 1_000_000)
        valToLedger = valFromLedger + bool zero valMint (mode == LedgerMode) - valWithdraw
    ledgerModifyTx par valToLedger

    -- Paying fees and withdrawing
    when (v < 0) $ do
        utxoProducedTx addrRelay    (protocolFee v mode) (Just inlinedUnit)
        utxoProducedTx addrTreasury (protocolFee v mode) (Just inlinedUnit)
        -- NOTE: withdrawing to a Plutus Script address is not possible
        utxoProducedTx changeAddr   valWithdraw          Nothing
