{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

module ENCOINS.Core.OnChain where

import           Ledger.Scripts                       (Versioned)
import           Ledger.Value                         (AssetClass)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude

import           ENCOINS.Bulletproofs                 (BulletproofSetup)
import           ENCOINS.Core.V1.OnChain              as V1
import           Scripts.OneShotCurrency              (OneShotCurrencyParams)

------------------------------------- Beacon Minting Policy --------------------------------------

{-# INLINABLE beaconTokenName #-}
beaconTokenName :: TokenName
beaconTokenName = V1.beaconTokenName

{-# INLINABLE beaconParams #-}
beaconParams :: TxOutRef -> OneShotCurrencyParams
beaconParams = V1.beaconParams

beaconPolicy :: TxOutRef -> MintingPolicy
beaconPolicy = V1.beaconPolicy

beaconPolicyV :: TxOutRef -> Versioned MintingPolicy
beaconPolicyV = V1.beaconPolicyV

beaconCurrencySymbol :: TxOutRef -> CurrencySymbol
beaconCurrencySymbol = V1.beaconCurrencySymbol

beaconAssetClass :: TxOutRef -> AssetClass
beaconAssetClass = V1.beaconAssetClass

beaconToken :: TxOutRef -> Value
beaconToken = V1.beaconToken

----------------------------------- ENCOINS Minting Policy ---------------------------------------

bulletproofSetup :: BulletproofSetup
bulletproofSetup = V1.bulletproofSetup

-- Beacon currency symbol and verifierPKH
type EncoinsParams = V1.EncoinsParams

type TxParams = V1.TxParams
type EncoinsInput = V1.EncoinsInput
type ProofSignature = V1.ProofSignature
type EncoinsRedeemer = V1.EncoinsRedeemer

{-# INLINABLE encoinName #-}
encoinName :: BuiltinByteString -> TokenName
encoinName = V1.encoinName

encoinsPolicyCheck :: V1.EncoinsParams -> V1.EncoinsRedeemer -> ScriptContext -> Bool
encoinsPolicyCheck = V1.encoinsPolicyCheck

encoinsPolicy :: V1.EncoinsParams -> MintingPolicy
encoinsPolicy = V1.encoinsPolicy

encoinsPolicyV :: V1.EncoinsParams -> Versioned MintingPolicy
encoinsPolicyV = V1.encoinsPolicyV

encoinsSymbol :: V1.EncoinsParams -> CurrencySymbol
encoinsSymbol = V1.encoinsSymbol

encoinsAssetClass :: V1.EncoinsParams -> BuiltinByteString -> AssetClass
encoinsAssetClass = V1.encoinsAssetClass

encoin :: V1.EncoinsParams -> BuiltinByteString -> Value
encoin = V1.encoin

------------------------------------- ADA Staking Validator --------------------------------------

-- ENCOINS currency symbol
type StakingParams = V1.StakingParams

{-# INLINABLE stakingValidatorCheck #-}
stakingValidatorCheck :: V1.StakingParams -> () -> () -> ScriptContext -> Bool
stakingValidatorCheck = V1.stakingValidatorCheck

stakingValidator :: V1.StakingParams -> Validator
stakingValidator = V1.stakingValidator

stakingValidatorV :: V1.StakingParams -> Versioned Validator
stakingValidatorV = V1.stakingValidatorV

stakingValidatorHash :: V1.StakingParams -> ValidatorHash
stakingValidatorHash = V1.stakingValidatorHash

stakingValidatorAddress :: V1.StakingParams -> Address
stakingValidatorAddress = V1.stakingValidatorAddress

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

{-# INLINABLE ledgerValidatorCheck #-}
ledgerValidatorCheck :: () -> () -> ScriptContext -> Bool
ledgerValidatorCheck = V1.ledgerValidatorCheck

ledgerValidator :: Validator
ledgerValidator = V1.ledgerValidator

ledgerValidatorV :: Versioned Validator
ledgerValidatorV = V1.ledgerValidatorV

ledgerValidatorHash :: ValidatorHash
ledgerValidatorHash = V1.ledgerValidatorHash

ledgerValidatorAddress :: Address
ledgerValidatorAddress = V1.ledgerValidatorAddress