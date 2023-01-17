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

import           Control.Monad.State                            (when)
import           Data.Functor                                   (($>), (<$>))
import           Data.Maybe                                     (fromJust)
import           Ledger                                         (DecoratedTxOut(..), _decoratedTxOutAddress)
import           Ledger.Ada                                     (lovelaceValueOf)
import           Ledger.Address                                 (PaymentPubKeyHash (..), toPubKeyHash, stakingCredential)
import           Ledger.Tokens                                  (token)
import           Ledger.Value                                   (AssetClass (..), geq, isAdaOnlyValue, gt, lt)
import           Plutus.Script.Utils.V2.Scripts                 (validatorHash, scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts           (validatorScript, validatorAddress)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))
import           Text.Hex                                       (decodeHex)

import           Constraints.OffChain
import           ENCOINS.BaseTypes                              (MintingPolarity (..))
import           ENCOINS.Bulletproofs                           (polarityToInteger)
import           ENCOINS.Core.V1.OnChain
import           Scripts.OneShotCurrency                        (oneShotCurrencyMintTx)
import           Types.Tx                                       (TransactionBuilder)

verifierPKH ::BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "FA729A50432E19737EEEEA0BFD8E673D41973E7ACE17A2EEDB2119F6F989108A"

------------------------------------- Beacon Minting Policy --------------------------------------

beaconCurrencySymbol :: TxOutRef -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconPolicy

beaconAssetClass :: TxOutRef -> AssetClass
beaconAssetClass ref = AssetClass (beaconCurrencySymbol ref, beaconTokenName)

beaconToken :: TxOutRef -> Value
beaconToken = token . beaconAssetClass

beaconMintTx :: TxOutRef -> TransactionBuilder ()
beaconMintTx ref = oneShotCurrencyMintTx (beaconParams ref) $> ()

beaconSendTx :: TxOutRef -> TransactionBuilder ()
beaconSendTx ref = utxoProducedScriptTx vh Nothing v ()
  where vh = stakingValidatorHash $ encoinsSymbol (beaconCurrencySymbol ref, verifierPKH)
        v  = beaconToken ref + lovelaceValueOf 2_000_000

----------------------------------- ENCOINS Minting Policy ---------------------------------------

encoinsSymbol :: EncoinsParams -> CurrencySymbol
encoinsSymbol = scriptCurrencySymbol . encoinsPolicy

encoinsAssetClass :: EncoinsParams -> BuiltinByteString -> AssetClass
encoinsAssetClass par a = AssetClass (encoinsSymbol par, encoinName a)

encoin :: EncoinsParams -> BuiltinByteString -> Value
encoin par a = token $ encoinsAssetClass par a

encoinsBurnTx :: EncoinsParams -> BuiltinByteString -> TransactionBuilder ()
encoinsBurnTx beaconSymb bs = do
    let f = \_ o -> _decoratedTxOutValue o `geq` encoin beaconSymb bs
    res1 <- utxoSpentPublicKeyTx' f
    res2 <- utxoSpentScriptTx' f (const . const $ ledgerValidator) (const . const $ ())
    failTx "encoinsBurnTx" "Cannot find the required coin" (res1 >> res2) $> ()

encoinsTx :: EncoinsParams -> EncoinsRedeemer -> TransactionBuilder ()
encoinsTx par@(beaconSymb, _) red@(addr, (v, inputs), _, _)  = do
    let beacon      = token (AssetClass (beaconSymb, beaconTokenName))
        coinsToBurn = filter (\(_, p) -> p == Burn) inputs
        val         = lovelaceValueOf (v * 1_000_000)
        valEncoins  = sum $ map (\(bs, p) -> scale (polarityToInteger p) (encoin par bs)) inputs
    mapM_ (encoinsBurnTx par . fst) coinsToBurn
    tokensMintedTx (encoinsPolicy par) red valEncoins
    stakingModifyTx (encoinsSymbol par) val
    when (v > 0) $
        utxoReferencedTx (\_ o -> _decoratedTxOutAddress o == addr && _decoratedTxOutValue o `geq` beacon) $> ()
    when (v < 0) $ fromMaybe (failTx "encoinsTx" "The address in the redeemer is not locked by a public key" Nothing $> ()) $ do
        pkh <- toPubKeyHash addr
        return $ utxoProducedPublicKeyTx (PaymentPubKeyHash pkh) (stakingCredential addr) (negate val) (Nothing :: Maybe ())

------------------------------------- ADA Staking Validator --------------------------------------

stakingValidator :: StakingParams -> Validator
stakingValidator = validatorScript . stakingTypedValidator

stakingValidatorHash :: StakingParams -> ValidatorHash
stakingValidatorHash = validatorHash . stakingValidator

stakingValidatorAddress :: StakingParams -> Address
stakingValidatorAddress = validatorAddress . stakingTypedValidator

-- Spend utxo greater than the given value from the Staking script.
stakingSpendTx' :: StakingParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx' par val =
    fmap (_decoratedTxOutValue . snd) <$> utxoSpentScriptTx'
        (\_ o -> _decoratedTxOutValue o `geq` val && isAdaOnlyValue (_decoratedTxOutValue o) && _decoratedTxOutAddress o == stakingValidatorAddress par)
        (const . const $ stakingValidator par) (const . const $ ())

-- Spend utxo greater than the given value from the Staking script. Fails if the utxo is not found.
stakingSpendTx :: StakingParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx par val = stakingSpendTx' par val >>= failTx "stakingSpendTx" "Cannot find a suitable utxo to spend"

-- Combines several utxos into one.
stakingCombineTx :: StakingParams -> Value -> Integer -> TransactionBuilder ()
stakingCombineTx par val 0 = utxoProducedScriptTx (stakingValidatorHash par) Nothing val ()
stakingCombineTx par val n = do
    res <- stakingSpendTx par zero
    let val' = val + fromMaybe zero res
    stakingCombineTx par val' (n-1)

-- Modify the value locked in staking by the given value
stakingModifyTx :: StakingParams -> Value -> TransactionBuilder ()
stakingModifyTx par val
    | val `gt` zero = utxoProducedScriptTx (stakingValidatorHash par) Nothing val ()
    | otherwise      = when (val `lt` zero) $ do
        res <- stakingSpendTx par val
        when (isJust res) $
            let valSpent  = fromJust res
                valChange = valSpent + val
            in when (valChange `gt` zero) $ utxoProducedScriptTx (stakingValidatorHash par) Nothing valChange ()

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

ledgerValidator :: Validator
ledgerValidator = validatorScript ledgerTypedValidator

ledgerValidatorHash :: ValidatorHash
ledgerValidatorHash = validatorHash ledgerValidator

ledgerValidatorAddress :: Address
ledgerValidatorAddress = validatorAddress ledgerTypedValidator

ledgerTx :: EncoinsParams -> [BuiltinByteString] -> TransactionBuilder ()
ledgerTx par gs =
    let vals = map (encoin par) gs
    in mapM_ (\val -> utxoSpentScriptTx (\_ o -> _decoratedTxOutValue o `geq` val)
        (const . const $ ledgerValidator) (const . const $ ())) vals
        