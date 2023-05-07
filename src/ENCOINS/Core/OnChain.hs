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

import qualified ENCOINS.Core.V1.OnChain                   as V1
import           PlutusAppsExtra.Scripts.OneShotCurrency   (OneShotCurrencyParams)

type EncoinsProtocolParams = V1.EncoinsProtocolParams

---------------------------- Stake Owner Token Minting Policy --------------------------------------

{-# INLINABLE stakeOwnerTokenName #-}
stakeOwnerTokenName :: TokenName
stakeOwnerTokenName = V1.stakeOwnerTokenName

{-# INLINABLE stakeOwnerMintParams #-}
stakeOwnerMintParams :: EncoinsProtocolParams -> OneShotCurrencyParams
stakeOwnerMintParams = V1.stakeOwnerMintParams

stakeOwnerPolicy :: EncoinsProtocolParams -> MintingPolicy
stakeOwnerPolicy = V1.stakeOwnerPolicy

stakeOwnerPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
stakeOwnerPolicyV = V1.stakeOwnerPolicyV

stakeOwnerCurrencySymbol :: EncoinsProtocolParams -> CurrencySymbol
stakeOwnerCurrencySymbol = V1.stakeOwnerCurrencySymbol

stakeOwnerAssetClass :: EncoinsProtocolParams -> AssetClass
stakeOwnerAssetClass = V1.stakeOwnerAssetClass

stakeOwnerToken :: EncoinsProtocolParams -> Value
stakeOwnerToken = V1.stakeOwnerToken

------------------------------------- Beacon Minting Policy --------------------------------------

{-# INLINABLE beaconTokenName #-}
beaconTokenName :: TokenName
beaconTokenName = V1.beaconTokenName

{-# INLINABLE beaconMintParams #-}
beaconMintParams :: EncoinsProtocolParams -> OneShotCurrencyParams
beaconMintParams = V1.beaconMintParams

beaconPolicy :: EncoinsProtocolParams -> MintingPolicy
beaconPolicy = V1.beaconPolicy

beaconPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
beaconPolicyV = V1.beaconPolicyV

beaconCurrencySymbol :: EncoinsProtocolParams -> CurrencySymbol
beaconCurrencySymbol = V1.beaconCurrencySymbol

beaconAssetClass :: EncoinsProtocolParams -> AssetClass
beaconAssetClass = V1.beaconAssetClass

beaconToken :: EncoinsProtocolParams -> Value
beaconToken = V1.beaconToken

----------------------------------- ENCOINS Minting Policy ---------------------------------------

-- Beacon currency symbol and verifierPKH
type EncoinsPolicyParams = V1.EncoinsPolicyParams

type TxParams = V1.TxParams
type EncoinsInput = V1.EncoinsInput
type ProofHash = V1.ProofHash
type ProofSignature = V1.ProofSignature
type EncoinsRedeemer = V1.EncoinsRedeemer
type EncoinsRedeemerOnChain = V1.EncoinsRedeemerOnChain

hashRedeemer :: EncoinsRedeemerOnChain -> BuiltinByteString
hashRedeemer = V1.hashRedeemer

{-# INLINABLE encoinName #-}
encoinName :: BuiltinByteString -> TokenName
encoinName = V1.encoinName

encoinsPolicyCheck :: EncoinsPolicyParams -> EncoinsRedeemerOnChain -> ScriptContext -> Bool
encoinsPolicyCheck = V1.encoinsPolicyCheck

toEncoinsPolicyParams :: EncoinsProtocolParams -> EncoinsPolicyParams
toEncoinsPolicyParams = V1.toEncoinsPolicyParams

encoinsPolicy :: EncoinsProtocolParams -> MintingPolicy
encoinsPolicy = V1.encoinsPolicy

encoinsPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
encoinsPolicyV = V1.encoinsPolicyV

encoinsSymbol :: EncoinsProtocolParams -> CurrencySymbol
encoinsSymbol = V1.encoinsSymbol

encoinsAssetClass :: EncoinsProtocolParams -> BuiltinByteString -> AssetClass
encoinsAssetClass = V1.encoinsAssetClass

encoin :: EncoinsProtocolParams -> BuiltinByteString -> Value
encoin = V1.encoin

encoinsInValue :: EncoinsProtocolParams -> Value -> [BuiltinByteString]
encoinsInValue = V1.encoinsInValue

--------------------------------------- ENCOINS Stake Validator ----------------------------------------

-- Stake owner currency symbol
type EncoinsStakeValidatorParams = V1.EncoinsStakeValidatorParams

encoinsStakeValidatorCheck :: EncoinsStakeValidatorParams -> () -> ScriptContext -> Bool
encoinsStakeValidatorCheck = V1.encoinsStakeValidatorCheck

encoinsStakeValidator :: EncoinsProtocolParams -> StakeValidator
encoinsStakeValidator = V1.encoinsStakeValidator

encoinsStakeValidatorV :: EncoinsProtocolParams -> Versioned StakeValidator
encoinsStakeValidatorV = V1.encoinsStakeValidatorV

encoinsStakeValidatorHash :: EncoinsProtocolParams -> StakeValidatorHash
encoinsStakeValidatorHash = V1.encoinsStakeValidatorHash

------------------------------------- ENCOINS Ledger Validator --------------------------------------

-- ENCOINS currency symbol
type EncoinsLedgerValidatorParams = V1.EncoinsLedgerValidatorParams

ledgerValidatorCheck :: EncoinsLedgerValidatorParams -> () -> () -> ScriptContext -> Bool
ledgerValidatorCheck = V1.ledgerValidatorCheck

ledgerValidator :: EncoinsProtocolParams -> Validator
ledgerValidator = V1.ledgerValidator

ledgerValidatorV :: EncoinsProtocolParams -> Versioned Validator
ledgerValidatorV = V1.ledgerValidatorV

ledgerValidatorHash :: EncoinsProtocolParams -> ValidatorHash
ledgerValidatorHash = V1.ledgerValidatorHash

ledgerValidatorAddress :: EncoinsProtocolParams -> Address
ledgerValidatorAddress = V1.ledgerValidatorAddress