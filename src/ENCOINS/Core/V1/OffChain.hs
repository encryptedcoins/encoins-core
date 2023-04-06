{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.V1.OffChain where

import           Control.Monad.State                        (when, gets)
import           Data.Bool                                  (bool)
import           Data.Functor                               (($>), (<$>))
import           Data.Maybe                                 (fromJust, catMaybes)
import           Data.Text                                  (pack)
import           Ledger                                     (DecoratedTxOut(..), _decoratedTxOutAddress)
import           Ledger.Ada                                 (lovelaceValueOf)
import           Ledger.Address                             (toPubKeyHash, stakingCredential)
import           Ledger.Tokens                              (token)
import           Ledger.Value                               (AssetClass (..), geq, isAdaOnlyValue, gt, lt)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                           hiding ((<$>), (<>), mapM)
import           Prelude                                    ((<>), mapM, show)
import           Text.Hex                                   (encodeHex)

import           ENCOINS.BaseTypes                          (MintingPolarity (..))
import           ENCOINS.Bulletproofs                       (polarityToInteger)
import           ENCOINS.Core.V1.OnChain
import           PlutusAppsExtra.Constraints.OffChain
import           PlutusAppsExtra.Scripts.CommonValidators   (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Scripts.OneShotCurrency    (oneShotCurrencyMintTx)
import           PlutusAppsExtra.Types.Tx                   (TransactionBuilder, TxConstructor (..))

protocolFee :: Integer -> Value
protocolFee n
    | n < 0 = lovelaceValueOf $ max 1_500_000 $ (negate n * 1_000_000) `divide` 200
    | otherwise = zero

---------------------------- Stake Owner Token Minting Policy --------------------------------------

stakeOwnerMintTx :: TxOutRef -> TransactionBuilder ()
stakeOwnerMintTx ref = oneShotCurrencyMintTx (stakeOwnerParams ref) $> ()

stakeOwnerTx :: TxOutRef -> TransactionBuilder ()
stakeOwnerTx = stakeOwnerMintTx

------------------------------------- Beacon Minting Policy --------------------------------------

beaconMintTx :: TxOutRef -> TransactionBuilder ()
beaconMintTx ref = oneShotCurrencyMintTx (beaconParams ref) $> ()

beaconSendTx :: TxOutRef -> BuiltinByteString -> TxOutRef -> TransactionBuilder ()
beaconSendTx refBeacon verifierPKH refOwner = utxoProducedTx (ledgerValidatorAddress (encoinsSymb, stakeOwnerSymb)) v (Just ())
  where encoinsSymb = encoinsSymbol (beaconCurrencySymbol refBeacon, verifierPKH)
        stakeOwnerSymb = stakeOwnerCurrencySymbol refOwner
        v  = beaconToken refBeacon + lovelaceValueOf 2_000_000

beaconTx :: TxOutRef -> BuiltinByteString -> TxOutRef -> TransactionBuilder ()
beaconTx refBeacon verifierPKH refOwner = do
    beaconMintTx refBeacon
    beaconSendTx refBeacon verifierPKH refOwner

----------------------------------- ENCOINS Minting Policy ---------------------------------------

type EncoinsRedeemerWithData = (Address, EncoinsRedeemer)

encoinsBurnTx :: EncoinsParams -> [BuiltinByteString] -> TransactionBuilder ()
encoinsBurnTx _   []       = return ()
encoinsBurnTx par (bs:bss) = do
    let f = \_ o -> _decoratedTxOutValue o `geq` encoin par bs
    res1 <- utxoSpentPublicKeyTx' f
    res2 <- utxoSpentScriptTx' f (const . const $ ledgerValidatorV $ encoinsSymbol par) (const . const $ ())
    -- At most one of the results is a Just.
    case bool res2 res1 (isJust res1) of
      Nothing -> failTx "encoinsBurnTx" ("Cannot find the required coin: " <> encodeHex (fromBuiltin bs)) Nothing $> ()
      Just (_, o) -> -- Filter out all encoins in the output and continue
        let bss' = filter (`notElem` encoinsInValue par (_decoratedTxOutValue o)) bss
        in encoinsBurnTx par bss'

encoinsTx :: (Address, Address) -> EncoinsParams -> EncoinsStakeParams -> EncoinsRedeemer -> Integer -> TransactionBuilder ()
encoinsTx (addrRelay, addrTreasury) par@(beaconSymb, _) stakeOwnerSymb red@((ledgerAddr, changeAddr), (v, inputs), _, _) n = do
    let beacon      = token (AssetClass (beaconSymb, beaconTokenName))
        coinsToBurn = filter (\(_, p) -> p == Burn) inputs
        val         = lovelaceValueOf (v * 1_000_000)
        valEncoins  = sum $ map (\(bs, p) -> scale (polarityToInteger p) (encoin par bs)) inputs
    encoinsBurnTx par $ map fst coinsToBurn
    tokensMintedTx (encoinsPolicyV par) red valEncoins
    ledgerModifyTx (encoinsSymbol par, stakeOwnerSymb) val n
    utxoReferencedTx (\_ o -> _decoratedTxOutAddress o == ledgerAddr && _decoratedTxOutValue o `geq` beacon) $> ()
    when (v < 0) $ fromMaybe (failTx "encoinsTx" "The address in the redeemer is not locked by a public key." Nothing $> ()) $ do
        pkh <- toPubKeyHash changeAddr
        return $ do
            utxoProducedTx addrRelay    (protocolFee v) (Just ())
            utxoProducedTx addrTreasury (protocolFee v) (Just ())
            utxoProducedPublicKeyTx pkh (stakingCredential changeAddr) (negate val) (Nothing :: Maybe ())

postEncoinsPolicyTx :: EncoinsParams -> Integer -> TransactionBuilder ()
postEncoinsPolicyTx par salt = postMintingPolicyTx (alwaysFalseValidatorAddress salt) (encoinsPolicyV par) (Just ()) zero

------------------------------------- ENCOINS Ledger Validator --------------------------------------

-- Spend utxo greater than the given value from the ENCOINS Ledger script.
ledgerSpendTx' :: EncoinsSpendParams -> Value -> TransactionBuilder (Maybe Value)
ledgerSpendTx' par@(parLedger, _) val =
    fmap (_decoratedTxOutValue . snd) <$> utxoSpentScriptTx'
        (\_ o -> _decoratedTxOutValue o `geq` val
        && isAdaOnlyValue (_decoratedTxOutValue o)
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
ledgerCombineTx par val 0 = utxoProducedTx (ledgerValidatorAddress par) val (Just ())
ledgerCombineTx par val n = do
    res <- ledgerSpendTx par zero
    let val' = val + fromMaybe zero res
    ledgerCombineTx par val' (n-1)

-- Modify the value locked in the ENCOINS Ledger script by the given value
ledgerModifyTx :: EncoinsSpendParams -> Value -> Integer -> TransactionBuilder ()
ledgerModifyTx par val n
    -- If we modify the value by 1 ADA, we must spend at least one utxo.
    | val == lovelaceValueOf 1_000_000 = do
        res  <- ledgerSpendTx par zero
        when (isJust res) $
            utxoProducedTx (ledgerValidatorAddress par) (fromJust res + val) (Just ())
    | val `gt` zero = utxoProducedTx (ledgerValidatorAddress par) val (Just ())
    | otherwise      = when (val `lt` zero) $ do
        -- TODO: Try spending several utxo to get the required value
        res  <- ledgerSpendTx par (negate val)
        -- TODO: Randomize the selection process
        res' <- mapM (const $ ledgerSpendTx' par zero) [1..n] -- Spend `n` additional utxos
        when (isJust res) $
            let valSpent  = fromJust res + sum (catMaybes res')
                valChange = valSpent + val
            in when (valChange `gt` zero) $ utxoProducedTx (ledgerValidatorAddress par) valChange (Just ())

postStakingValidatorTx :: EncoinsLedgerParams -> Integer -> TransactionBuilder ()
postStakingValidatorTx par salt = postValidatorTx (alwaysFalseValidatorAddress salt) (ledgerValidatorV par) (Just ()) zero