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

import           Ledger.Scripts                            (Versioned)
import           Ledger.Value                              (AssetClass)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude

import           ENCOINS.Core.V1.OnChain                   as V1
import           PlutusAppsExtra.Scripts.OneShotCurrency   (OneShotCurrencyParams)

---------------------------- Stake Owner Token Minting Policy --------------------------------------

{-# INLINABLE stakeOwnerTokenName #-}
stakeOwnerTokenName :: TokenName
stakeOwnerTokenName = V1.stakeOwnerTokenName

{-# INLINABLE stakeOwnerParams #-}
stakeOwnerParams :: TxOutRef -> OneShotCurrencyParams
stakeOwnerParams = V1.stakeOwnerParams

stakeOwnerPolicy :: TxOutRef -> MintingPolicy
stakeOwnerPolicy = V1.stakeOwnerPolicy

stakeOwnerPolicyV :: TxOutRef -> Versioned MintingPolicy
stakeOwnerPolicyV = V1.stakeOwnerPolicyV

stakeOwnerCurrencySymbol :: TxOutRef -> CurrencySymbol
stakeOwnerCurrencySymbol = V1.stakeOwnerCurrencySymbol

stakeOwnerAssetClass :: TxOutRef -> AssetClass
stakeOwnerAssetClass = V1.stakeOwnerAssetClass

stakeOwnerToken :: TxOutRef -> Value
stakeOwnerToken = V1.stakeOwnerToken

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

-- Beacon currency symbol and verifierPKH
type EncoinsParams = V1.EncoinsParams

type TxParams = V1.TxParams
type EncoinsInput = V1.EncoinsInput
type ProofSignature = V1.ProofSignature
type EncoinsRedeemer = V1.EncoinsRedeemer

hashRedeemer :: V1.EncoinsRedeemer -> BuiltinByteString
hashRedeemer = V1.hashRedeemer

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

encoinsInValue :: V1.EncoinsParams -> Value -> [BuiltinByteString]
encoinsInValue = V1.encoinsInValue

--------------------------------------- ENCOINS Stake Validator ----------------------------------------

-- Stake owner currency symbol
type EncoinsStakeParams = V1.EncoinsStakeParams

encoinsStakeValidatorCheck :: V1.EncoinsStakeParams -> () -> ScriptContext -> Bool
encoinsStakeValidatorCheck = V1.encoinsStakeValidatorCheck

encoinsStakeValidator :: V1.EncoinsStakeParams -> StakeValidator
encoinsStakeValidator = V1.encoinsStakeValidator

encoinsStakeValidatorV :: V1.EncoinsStakeParams -> Versioned StakeValidator
encoinsStakeValidatorV = V1.encoinsStakeValidatorV

encoinsStakeValidatorHash :: V1.EncoinsStakeParams -> StakeValidatorHash
encoinsStakeValidatorHash = V1.encoinsStakeValidatorHash

------------------------------------- ENCOINS Ledger Validator --------------------------------------

-- ENCOINS currency symbol
type EncoinsLedgerParams = V1.EncoinsLedgerParams

ledgerValidatorCheck :: V1.EncoinsLedgerParams -> () -> () -> ScriptContext -> Bool
ledgerValidatorCheck = V1.ledgerValidatorCheck

ledgerValidator :: V1.EncoinsLedgerParams -> Validator
ledgerValidator = V1.ledgerValidator

ledgerValidatorV :: V1.EncoinsLedgerParams -> Versioned Validator
ledgerValidatorV = V1.ledgerValidatorV

ledgerValidatorHash :: V1.EncoinsLedgerParams -> ValidatorHash
ledgerValidatorHash = V1.ledgerValidatorHash

type EncoinsSpendParams = V1.EncoinsSpendParams

ledgerValidatorAddress :: V1.EncoinsSpendParams -> Address
ledgerValidatorAddress = V1.ledgerValidatorAddress