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
import           Ledger                                   (DecoratedTxOut (..), _decoratedTxOutAddress, maxMinAdaTxOut)
import           Ledger.Ada                               (lovelaceValueOf, toValue)
import           Ledger.Address                           (stakingCredential, toPubKeyHash)
import           Ledger.Tokens                            (token)
import           Ledger.Value                             (AssetClass (..), adaOnlyValue, geq, gt, leq)
import           Plutus.V2.Ledger.Api                     hiding (singleton)
import           PlutusTx.AssocMap                        (lookup, singleton)
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

data EncoinsMode = WalletMode | LedgerMode
    deriving (Haskell.Show, Haskell.Read, Haskell.Eq, Generic, FromJSON, ToJSON)

instance Eq EncoinsMode where
    (==) = (Haskell.==)

protocolFee :: Integer -> EncoinsMode -> Value
protocolFee n mode
    | n < 0 || mode == LedgerMode = lovelaceValueOf $ max f $ (negate n * 1_000_000) `divide` 200
    | otherwise = zero
    where f = case mode of
            WalletMode -> 1_500_000
            LedgerMode -> 2_000_000

---------------------------- Stake Owner Token Minting Policy --------------------------------------

stakeOwnerMintTx :: TxOutRef -> TransactionBuilder ()
stakeOwnerMintTx ref = oneShotCurrencyMintTx (stakeOwnerParams ref) $> ()

stakeOwnerTx :: TxOutRef -> TransactionBuilder ()
stakeOwnerTx = stakeOwnerMintTx

------------------------------------- Beacon Minting Policy --------------------------------------

beaconMintTx :: TxOutRef -> TransactionBuilder ()
beaconMintTx ref = oneShotCurrencyMintTx (beaconParams ref) $> ()

beaconSendTx :: TxOutRef -> BuiltinByteString -> TxOutRef -> TransactionBuilder ()
beaconSendTx refBeacon verifierPKH refOwner = utxoProducedTx (ledgerValidatorAddress (encoinsSymb, stakeOwnerSymb)) v (Just hashedUnit)
  where encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
        stakeOwnerSymb = stakeOwnerCurrencySymbol refOwner
        v  = beaconToken refBeacon + lovelaceValueOf 2_000_000

beaconTx :: TxOutRef -> BuiltinByteString -> TxOutRef -> TransactionBuilder ()
beaconTx refBeacon verifierPKH refOwner = do
    beaconMintTx refBeacon
    beaconSendTx refBeacon verifierPKH refOwner

----------------------------------- ENCOINS Minting Policy ---------------------------------------

-- Returns value spent from the ENCOINS Ledger.
encoinsBurnTx :: EncoinsParams -> [BuiltinByteString] -> EncoinsMode -> TransactionBuilder Value
encoinsBurnTx _   []  _    = return zero
encoinsBurnTx par bss mode = do
    let bs = head bss
        f  = \_ o -> _decoratedTxOutValue o `geq` encoin par bs
    res1 <- utxoSpentPublicKeyTx' f
    res2 <- utxoSpentScriptTx' f (const . const $ ledgerValidatorV $ encoinsSymbol par) (const . const $ ())
    -- At most one of the results is a Just.
    case bool res2 res1 (isJust res1) of
      Nothing -> failTx "encoinsBurnTx" ("Cannot find the required coin: " <> encodeHex (fromBuiltin bs)) Nothing $> zero
      Just (_, o) ->
        -- Filter out all encoins in the output
        let bss' = filter (`notElem` encoinsInValue par (_decoratedTxOutValue o)) bss
        -- Sum the current and the future values spent from the ENCOINS Ledger
        in (+) (maybe zero (_decoratedTxOutValue . snd) res2) <$> encoinsBurnTx par bss' mode

encoinsTx :: (Address, Address) -> EncoinsParams -> EncoinsStakeParams -> EncoinsRedeemer -> EncoinsMode -> TransactionBuilder ()
encoinsTx (addrRelay, addrTreasury) par@(beaconSymb, _) stakeOwnerSymb red@((ledgerAddr, changeAddr), (v, inputs), _, _) mode = do
    let beacon      = token (AssetClass (beaconSymb, beaconTokenName))
        coinsToBurn = filter (\(_, p) -> p == Burn) inputs
        val         = lovelaceValueOf (v * 1_000_000)
        valEncoins  = sum $ map (\(bs, p) -> scale (polarityToInteger p) (encoin par bs)) inputs
    val' <- encoinsBurnTx par (map fst coinsToBurn) mode
    tokensMintedTx (encoinsPolicyV par) red valEncoins
    let val'' = val + bool zero (val' + valEncoins) (mode == LedgerMode)
    ledgerModifyTx (encoinsSymbol par, stakeOwnerSymb) val''
    utxoReferencedTx (\_ o -> _decoratedTxOutAddress o == ledgerAddr && _decoratedTxOutValue o `geq` beacon) $> ()
    when (v < 0) $ fromMaybe (failTx "encoinsTx" "The address in the redeemer is not locked by a public key." Nothing $> ()) $ do
        pkh <- toPubKeyHash changeAddr
        return $ do
            utxoProducedTx addrRelay    (protocolFee v mode) (Just inlinedUnit)
            utxoProducedTx addrTreasury (protocolFee v mode) (Just inlinedUnit)
            utxoProducedPublicKeyTx pkh (stakingCredential changeAddr) (negate val) Nothing

postEncoinsPolicyTx :: EncoinsParams -> Integer -> TransactionBuilder ()
postEncoinsPolicyTx par salt = postMintingPolicyTx (alwaysFalseValidatorAddress salt) (encoinsPolicyV par) (Just inlinedUnit) zero

------------------------------------- ENCOINS Ledger Validator --------------------------------------

encoinsOnlyValue :: EncoinsLedgerParams -> Value -> Value
encoinsOnlyValue symb = maybe zero (Value . singleton symb) . lookup symb . getValue

isEncoinsAndAdaOnlyValue :: EncoinsLedgerParams -> Value -> Bool
isEncoinsAndAdaOnlyValue symb val = val == adaOnlyValue val + encoinsOnlyValue symb val

-- Spend utxo greater than the given value from the ENCOINS Ledger script.
ledgerSpendTx' :: EncoinsSpendParams -> Value -> TransactionBuilder (Maybe Value)
ledgerSpendTx' par@(parLedger, _) val =
    fmap (_decoratedTxOutValue . snd) <$> utxoSpentScriptTx'
        (\_ o -> _decoratedTxOutValue o `geq` val
        && isEncoinsAndAdaOnlyValue parLedger (_decoratedTxOutValue o)
        && _decoratedTxOutAddress o == ledgerValidatorAddress par)
        (const . const $ ledgerValidatorV parLedger) (const . const $ ())

-- Spend utxo greater than the given value from the ENCOINS Ledger script. Fails if the utxo is not found.
ledgerSpendTx :: EncoinsSpendParams -> Value -> TransactionBuilder (Maybe Value)
ledgerSpendTx par val = do
    res <- ledgerSpendTx' par val
    if isNothing res
        then do
            utxos <- gets txConstructorLookups
            failTx "ledgerSpendTx" ("Cannot find a suitable utxo to spend. UTXOs: " <> pack (show utxos)) res
        else return res

-- Combines several utxos into one.
ledgerCombineTx :: EncoinsSpendParams -> Value -> Integer -> TransactionBuilder ()
ledgerCombineTx par val 0 = utxoProducedTx (ledgerValidatorAddress par) val (Just inlinedUnit)
ledgerCombineTx par val n = do
    res <- ledgerSpendTx par zero
    let val' = val + fromMaybe zero res
    ledgerCombineTx par val' (n-1)

-- Modify the value locked in the ENCOINS Ledger script by the given value
ledgerModifyTx :: EncoinsSpendParams -> Value -> TransactionBuilder ()
ledgerModifyTx par val
    -- If we modify the value by 1 ADA, we must spend at least one utxo.
    | adaOnlyValue val `leq` toValue maxMinAdaTxOut && adaOnlyValue val `gt` zero = do
        val' <- fromMaybe zero <$> ledgerSpendTx par zero
        ledgerModifyTx par (val + val')
    | val `gt` zero = utxoProducedTx (ledgerValidatorAddress par) val (Just inlinedUnit)
    | otherwise     = do
        let valAda = negate $ adaOnlyValue val
        -- TODO: Try spending several utxo to get the required value
        val'  <- fromMaybe zero <$> ledgerSpendTx par valAda
        -- TODO: Randomize the selection process
        val'' <- fromMaybe zero <$> ledgerSpendTx' par zero -- Spend an additional utxo
        ledgerModifyTx par (val + val' + val'')

postStakingValidatorTx :: EncoinsLedgerParams -> Integer -> TransactionBuilder ()
postStakingValidatorTx par salt = postValidatorTx (alwaysFalseValidatorAddress salt) (ledgerValidatorV par) (Just inlinedUnit) zero
