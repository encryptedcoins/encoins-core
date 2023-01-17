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

module ENCOINS.Core.OffChain where

import           Control.Monad.State                            (when)
import           Data.Functor                                   (($>), (<$>))
import           Data.Maybe                                     (fromJust)
import           Ledger                                         (DecoratedTxOut(..), _decoratedTxOutAddress)
import           Ledger.Ada                                     (lovelaceValueOf)
import           Ledger.Address                                 (PaymentPubKeyHash (..))
import           Ledger.Tokens                                  (token)
import           Ledger.Value                                   (AssetClass (..), geq, isAdaOnlyValue, gt, lt)
import           Plutus.Script.Utils.V2.Scripts                 (validatorHash, scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts           (validatorScript, validatorAddress)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           Constraints.OffChain
import           ENCOINS.Core.BaseTypes                         (MintingPolarity (..))
import           ENCOINS.Core.Bulletproofs.Utils                (polarityToInteger)
import           ENCOINS.Core.OnChain                           (EncoinsParams, EncoinsRedeemer, StakingParams, encoinName, encoinsPolicy,
                                                                    stakingTypedValidator, beaconPolicy, beaconTokenName, beaconParams, ledgerTypedValidator)
import           Scripts.OneShotCurrency                        (oneShotCurrencyMintTx)
import           Types.Tx                                       (TransactionBuilder)

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
  where vh = stakingValidatorHash $ encoinsSymbol $ beaconCurrencySymbol ref
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
    failTx "encoinsBurnTx" "utxoSpentPublicKeyTx' or utxoSpentScriptTx' failed" (res1 >> res2) $> ()

-- TODO: finish implementation
encoinsTx :: EncoinsParams -> EncoinsRedeemer -> TransactionBuilder ()
encoinsTx beaconSymb red@((v, _, pkh, (_, _)), inputs, _)  = do
    let -- beacon      = token (AssetClass (beaconSymb, beaconTokenName))
        coinsToBurn = filter (\(_, p) -> p == Burn) inputs
        val         = lovelaceValueOf (v * 1_000_000)
        valEncoins  = sum $ map (\(bs, p) -> scale (polarityToInteger p) (encoin beaconSymb (sha2_256 bs))) inputs
    mapM_ (encoinsBurnTx beaconSymb . fst) coinsToBurn
    tokensMintedTx (encoinsPolicy beaconSymb) red valEncoins
    stakingModifyTx (encoinsSymbol beaconSymb) val
    -- validatedInIntervalTx tFrom tTo
    mustBeSignedByTx $ PaymentPubKeyHash pkh
    -- utxoReferencedTx (\_ o -> _decoratedTxOutAddress o == addr && _decoratedTxOutValue o `geq` beacon) $> ()

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
stakingSpendTx par val = stakingSpendTx' par val >>= failTx "stakingSpendTx" "No matching utxos found"

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
        