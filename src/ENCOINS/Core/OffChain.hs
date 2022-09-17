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
import           Data.Functor                                   (($>))
import           Ledger                                         (ChainIndexTxOut(..))
import           Ledger.Ada                                     (lovelaceValueOf)
import           Ledger.Tokens                                  (token)
import           Ledger.Typed.Scripts                           (Any)
import           Ledger.Value                                   (AssetClass (..), geq, isAdaOnlyValue)
import           Plutus.Contract.StateMachine.MintingPolarity   (MintingPolarity(..))
import           Plutus.Script.Utils.V2.Scripts                 (validatorHash, scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts           (ValidatorTypes (..), validatorScript, validatorAddress)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude
import           Prelude                                        (undefined)

import           ENCOINS.Core.Bulletproofs                      (Input, polarityToInteger)
import           ENCOINS.Core.OnChain                           (EncoinsParams, EncoinsRedeemer, StakingParams, encoinName, encoinsPolicy,
                                                                    stakingTypedValidator, beaconPolicy, beaconTokenName, beaconParams)
import           ENCOINS.Core.Types                             (GroupElement, fromFieldElement)
import           Scripts.OneShotCurrency                        (oneShotCurrencyMintTx)
import           Scripts.Constraints                  
import           Types.TxConstructor                            (TxConstructor (..))


type EncoinsTransaction = TxConstructor () Any (RedeemerType Any) (DatumType Any)
type EncoinsTransactionBuilder = State EncoinsTransaction ()

------------------------------------- Beacon Minting Policy --------------------------------------

beaconCurrencySymbol :: TxOutRef -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconPolicy

beaconAssetClass :: TxOutRef -> AssetClass
beaconAssetClass ref = AssetClass (beaconCurrencySymbol ref, beaconTokenName)

beaconToken :: TxOutRef -> Value
beaconToken = token . beaconAssetClass

beaconMintTx :: TxOutRef -> EncoinsTransactionBuilder
beaconMintTx ref = oneShotCurrencyMintTx (beaconParams ref) $> ()

beaconSendTx :: TxOutRef -> EncoinsTransactionBuilder
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

encoinsMintTx :: EncoinsParams -> EncoinsRedeemer -> () -> EncoinsTransactionBuilder
encoinsMintTx beaconSymb red@(addr, (coins, v), _) () = do
    let beacon = token (AssetClass (beaconSymb, beaconTokenName))
    tokensMintedTx (encoinsPolicy beaconSymb) red (sum $ map (\(g, p) -> scale (polarityToInteger p) (encoin beaconSymb g)) coins)
    stakingWithdrawTx (encoinsSymbol beaconSymb) (coins, v)
    utxoReferencedTx (\_ o -> _ciTxOutAddress o == addr && _ciTxOutValue o `geq` beacon) $> ()

------------------------------------- ADA Staking Validator --------------------------------------

stakingValidator :: StakingParams -> Validator
stakingValidator = validatorScript . stakingTypedValidator

stakingValidatorHash :: StakingParams -> ValidatorHash
stakingValidatorHash = validatorHash . stakingValidator

stakingValidatorAddress :: StakingParams -> Address
stakingValidatorAddress = validatorAddress . stakingTypedValidator

-- TODO: implement this, simplify stakingWithdrawTx
stakingSpendTx :: StakingParams -> Integer -> EncoinsTransactionBuilder
stakingSpendTx _ _ = undefined

-- TODO: add ENCOINS Ledger support here
stakingWithdrawTx :: StakingParams -> Input -> EncoinsTransactionBuilder
stakingWithdrawTx par (coins, v) = do
    -- burn from the user's wallet
    let coinsToBurn = filter (\(_, p) -> p == Burn) coins
    mapM_ (\c -> utxoSpentPublicKeyTx (\_ o -> _ciTxOutValue o `geq` encoin par (fst c))) coinsToBurn
    let vStake = fromFieldElement v
    if vStake > 0
        then utxoProducedScriptTx (stakingValidatorHash par) Nothing (lovelaceValueOf vStake) ()
        else if vStake < 0
            then do
            res <- utxoSpentScriptTx (\_ o -> _ciTxOutValue o `geq` lovelaceValueOf vStake && isAdaOnlyValue (_ciTxOutValue o))
                (const . const $ stakingValidator par) (const . const $ ())
            case res of
              Nothing     -> failTx Nothing $> ()
              Just (_, o) -> 
                let valChange = _ciTxOutValue o - lovelaceValueOf vStake
                in utxoProducedScriptTx (stakingValidatorHash par) Nothing valChange ()
            else return ()