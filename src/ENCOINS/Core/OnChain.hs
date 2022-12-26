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

import           Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator)
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

----------------------------------- ENCOINS Minting Policy ---------------------------------------

bulletproofSetup :: BulletproofSetup
bulletproofSetup = V1.bulletproofSetup

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

------------------------------------- ADA Staking Validator --------------------------------------

type StakingParams = V1.StakingParams

type StakingADA = V1.StakingADA

{-# INLINABLE stakingValidatorCheck #-}
stakingValidatorCheck :: V1.StakingParams -> () -> () -> ScriptContext -> Bool
stakingValidatorCheck = V1.stakingValidatorCheck

stakingTypedValidator :: V1.StakingParams -> TypedValidator V1.StakingADA
stakingTypedValidator = V1.stakingTypedValidator

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

type Ledgering = V1.Ledgering

{-# INLINABLE ledgerValidatorCheck #-}
ledgerValidatorCheck :: () -> () -> ScriptContext -> Bool
ledgerValidatorCheck = V1.ledgerValidatorCheck

ledgerTypedValidator :: TypedValidator V1.Ledgering
ledgerTypedValidator = V1.ledgerTypedValidator
