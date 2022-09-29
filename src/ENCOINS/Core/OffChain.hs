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

import           Control.Monad.State                            (State)
import           Data.Functor                                   (($>), (<$>))
import           Ledger                                         (ChainIndexTxOut(..))
import           Ledger.Ada                                     (lovelaceValueOf)
import           Ledger.Tokens                                  (token)
import           Ledger.Typed.Scripts                           (Any)
import           Ledger.Value                                   (AssetClass (..), geq, isAdaOnlyValue)
import           Plutus.Script.Utils.V2.Scripts                 (validatorHash, scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts           (ValidatorTypes (..), validatorScript, validatorAddress)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.Core.OnChain                           (EncoinsParams, EncoinsRedeemer, StakingParams, encoinName, encoinsPolicy,
                                                                    stakingTypedValidator, beaconPolicy, beaconTokenName, beaconParams, ledgerTypedValidator)
import           ENCOINS.Core.Types                             (GroupElement, MintingPolarity (..), polarityToInteger)
import           Scripts.OneShotCurrency                        (oneShotCurrencyMintTx)
import           Scripts.Constraints
import           Types.TxConstructor                            (TxConstructor (..))


type EncoinsTransaction = TxConstructor () Any (RedeemerType Any) (DatumType Any)
type EncoinsTransactionBuilder a = State EncoinsTransaction a

------------------------------------- Beacon Minting Policy --------------------------------------

beaconCurrencySymbol :: TxOutRef -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconPolicy

beaconAssetClass :: TxOutRef -> AssetClass
beaconAssetClass ref = AssetClass (beaconCurrencySymbol ref, beaconTokenName)

beaconToken :: TxOutRef -> Value
beaconToken = token . beaconAssetClass

beaconMintTx :: TxOutRef -> EncoinsTransactionBuilder ()
beaconMintTx ref = oneShotCurrencyMintTx (beaconParams ref) $> ()

beaconSendTx :: TxOutRef -> EncoinsTransactionBuilder ()
beaconSendTx ref = utxoProducedScriptTx vh Nothing v ()
  where vh = stakingValidatorHash $ encoinsSymbol $ beaconCurrencySymbol ref
        v  = beaconToken ref + lovelaceValueOf 2_000_000

----------------------------------- ENCOINS Minting Policy ---------------------------------------

encoinsSymbol :: EncoinsParams -> CurrencySymbol
encoinsSymbol = scriptCurrencySymbol . encoinsPolicy

encoinsAssetClass :: EncoinsParams -> GroupElement -> AssetClass
encoinsAssetClass par a = AssetClass (encoinsSymbol par, encoinName a)

encoin :: EncoinsParams -> GroupElement -> Value
encoin par a = token $ encoinsAssetClass par a

encoinsBurnTx :: EncoinsParams -> GroupElement -> EncoinsTransactionBuilder ()
encoinsBurnTx beaconSymb g = do
    let f = \_ o -> _ciTxOutValue o `geq` encoin beaconSymb g
    _ <- utxoSpentPublicKeyTx' f
    _ <- utxoSpentScriptTx' f (const . const $ ledgerValidator) (const . const $ ())
    return ()

encoinsTx :: EncoinsParams -> EncoinsRedeemer -> EncoinsTransactionBuilder ()
encoinsTx beaconSymb red@(addr, (coins, v), _) = do
    let beacon = token (AssetClass (beaconSymb, beaconTokenName))
        coinsToBurn = filter (\(_, p) -> p == Burn) coins
        val = lovelaceValueOf v
    mapM_ (encoinsBurnTx beaconSymb . fst) coinsToBurn
    tokensMintedTx (encoinsPolicy beaconSymb) red (sum $ map (\(g, p) -> scale (polarityToInteger p) (encoin beaconSymb g)) coins)
    stakingModifyTx (encoinsSymbol beaconSymb) val
    utxoReferencedTx (\_ o -> _ciTxOutAddress o == addr && _ciTxOutValue o `geq` beacon) $> ()

------------------------------------- ADA Staking Validator --------------------------------------

stakingValidator :: StakingParams -> Validator
stakingValidator = validatorScript . stakingTypedValidator

stakingValidatorHash :: StakingParams -> ValidatorHash
stakingValidatorHash = validatorHash . stakingValidator

stakingValidatorAddress :: StakingParams -> Address
stakingValidatorAddress = validatorAddress . stakingTypedValidator

-- Spend utxo greater than the given value.
stakingSpendTx' :: StakingParams -> Value -> EncoinsTransactionBuilder (Maybe Value)
stakingSpendTx' par val =
    fmap (_ciTxOutValue . snd) <$> utxoSpentScriptTx' (\_ o -> _ciTxOutValue o `geq` val && isAdaOnlyValue (_ciTxOutValue o))
        (const . const $ stakingValidator par) (const . const $ ())

-- Spend utxo greater than the given value. Fails if the utxo is not found.
stakingSpendTx :: StakingParams -> Value -> EncoinsTransactionBuilder (Maybe Value)
stakingSpendTx par val = stakingSpendTx' par val >>= failTx

-- Combines several utxos into one.
stakingCombineTx :: StakingParams -> Value -> Integer -> EncoinsTransactionBuilder ()
stakingCombineTx par val 0 = utxoProducedScriptTx (stakingValidatorHash par) Nothing val ()
stakingCombineTx par val n = do
    res <- stakingSpendTx par zero
    let val' = val + fromMaybe zero res
    stakingCombineTx par val' (n-1)

-- Modify the value locked in staking by the given value
stakingModifyTx :: StakingParams -> Value -> EncoinsTransactionBuilder ()
stakingModifyTx par val
    | val `geq` zero = utxoProducedScriptTx (stakingValidatorHash par) Nothing val ()
    | val == zero    = return ()
    | otherwise      = do
        res <- stakingSpendTx par val
        case res of
            Nothing     -> failTx Nothing $> ()
            Just valSpent ->
                let valChange = valSpent - val
                in utxoProducedScriptTx (stakingValidatorHash par) Nothing valChange ()

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

ledgerValidator :: Validator
ledgerValidator = validatorScript ledgerTypedValidator

ledgerValidatorHash :: ValidatorHash
ledgerValidatorHash = validatorHash ledgerValidator

ledgerValidatorAddress :: Address
ledgerValidatorAddress = validatorAddress ledgerTypedValidator

ledgerTx :: EncoinsParams -> [GroupElement] -> EncoinsTransactionBuilder ()
ledgerTx par gs =
    let vals = map (encoin par) gs
    in mapM_ (\val -> utxoSpentScriptTx (\_ o -> _ciTxOutValue o `geq` val)
        (const . const $ ledgerValidator) (const . const $ ())) vals
        