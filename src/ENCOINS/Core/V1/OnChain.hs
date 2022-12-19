{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

module ENCOINS.Core.V1.OnChain where

import           Ledger.Ada                           (lovelaceValueOf)
import           Ledger.Tokens                        (token)
import           Ledger.Value                         (geq, noAdaValue, AssetClass (..))
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), TypedValidator,
                                                        mkTypedValidator, mkUntypedValidator, mkUntypedMintingPolicy)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            (findOwnInput)
import           PlutusTx                             (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                    (lookup)
import           PlutusTx.Prelude

import           Constraints.OnChain                  (tokensMinted, filterUtxoSpent, filterUtxoProduced, utxoReferenced)
import           ENCOINS.Bulletproofs
import           ENCOINS.BaseTypes
import           ENCOINS.Crypto.Field
import           PlutusTx.Extra.ByteString            (ToBuiltinByteString(..))
import           Scripts.OneShotCurrency              (OneShotCurrencyParams, mkCurrency, oneShotCurrencyPolicy)
import           Utils.Orphans                        ()

------------------------------------- Beacon Minting Policy --------------------------------------

{-# INLINABLE beaconTokenName #-}
beaconTokenName :: TokenName
beaconTokenName = TokenName emptyByteString

{-# INLINABLE beaconParams #-}
beaconParams :: TxOutRef -> OneShotCurrencyParams
beaconParams ref = mkCurrency ref [(beaconTokenName, 1)]

beaconPolicy :: TxOutRef -> MintingPolicy
beaconPolicy = oneShotCurrencyPolicy . beaconParams

----------------------------------- ENCOINS Minting Policy ---------------------------------------

bulletproofSetup :: BulletproofSetup
bulletproofSetup = BulletproofSetup groupGenerator (groupExp groupGenerator (toFieldElement 2))
  (map (groupExp groupGenerator . toFieldElement) [3..(bulletproofN * bulletproofM + 2)])
  (map (groupExp groupGenerator . toFieldElement) [(bulletproofN * bulletproofM + 3)..(2 * (bulletproofN * bulletproofM) + 2)])

verifierPKH :: BuiltinByteString
verifierPKH = emptyByteString

-- Beacon currency symbol
type EncoinsParams = CurrencySymbol

type TxParams = Address
type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)

hashRedeemer :: EncoinsRedeemer -> BuiltinByteString
hashRedeemer (addr, (v, inputs), proof, _) = sha2_256 $ toBytes addr `appendByteString` toBytes (v, inputs) `appendByteString` toBytes proof

{-# INLINABLE encoinName #-}
encoinName :: BuiltinByteString -> TokenName
encoinName = TokenName . sha2_256

-- TODO: remove on-chain sorting (requires sorting inputs and proof components)
encoinsPolicyCheck :: EncoinsParams -> EncoinsRedeemer -> ScriptContext -> Bool
encoinsPolicyCheck beaconSymb red@(addr, (v, inputs), _, sig)
    ctx@ScriptContext{scriptContextTxInfo=info} =
      cond0
      && cond1
      && cond2
      && cond3
  where
      beacon = token (AssetClass (beaconSymb, beaconTokenName))
      val    = lovelaceValueOf (abs v * 1_000_000)

      cond0 = tokensMinted ctx $ fromList $ sort $ map (\(bs, p) -> (encoinName bs, polarityToInteger p)) inputs
      cond1 = verifyEd25519Signature verifierPKH (hashRedeemer red) sig
      cond2 = sum (map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == addr)) `geq` val
      cond3 = utxoReferenced info (\o -> txOutAddress o == addr && txOutValue o `geq` beacon) || (v <= 0)

encoinsPolicy :: EncoinsParams -> MintingPolicy
encoinsPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . encoinsPolicyCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

------------------------------------- ADA Staking Validator --------------------------------------

-- ENCOINS currency symbol
type StakingParams = CurrencySymbol

data StakingADA
instance ValidatorTypes StakingADA where
  type instance DatumType StakingADA = ()
  type instance RedeemerType StakingADA = ()

{-# INLINABLE stakingValidatorCheck #-}
stakingValidatorCheck :: StakingParams -> () -> () -> ScriptContext -> Bool
stakingValidatorCheck encoinsSymb _ _
    ctx@ScriptContext{scriptContextTxInfo=info} = cond0
  where
    purp = Minting encoinsSymb
    addr = txOutAddress $ txInInfoResolved $ fromMaybe (error ()) $ findOwnInput ctx -- this script's address
    vOut = sum $ map txOutValue $ filterUtxoSpent info (\o -> txOutAddress o == addr)
    vIn  = sum $ map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == addr)
    val  = lovelaceValueOf $ (* 1_000_000) $ fromMaybe 0 $ do
      red <- lookup purp $ txInfoRedeemers info
      (_, (v, _), _, _) <- fromBuiltinData $ getRedeemer red :: Maybe EncoinsRedeemer
      Just v

    cond0 = vIn == (vOut + val)

stakingTypedValidator :: StakingParams -> TypedValidator StakingADA
stakingTypedValidator par = mkTypedValidator @StakingADA
    ($$(PlutusTx.compile [|| stakingValidatorCheck ||])
    `PlutusTx.applyCode` PlutusTx.liftCode par)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @() @()

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

data Ledgering
instance ValidatorTypes Ledgering where
  type instance DatumType Ledgering = ()
  type instance RedeemerType Ledgering = ()

{-# INLINABLE ledgerValidatorCheck #-}
ledgerValidatorCheck :: () -> () -> ScriptContext -> Bool
ledgerValidatorCheck _ _
    ctx@ScriptContext{scriptContextTxInfo=info} = cond0
  where
    addr  = txOutAddress $ txInInfoResolved $ fromMaybe (error ()) $ findOwnInput ctx -- this script's address
    vOut  = noAdaValue $ sum $ map txOutValue $ filterUtxoSpent info (\o -> txOutAddress o == addr)
    vIn   = noAdaValue $ sum $ map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == addr)
    vMint = txInfoMint $ scriptContextTxInfo ctx

    cond0 = vIn == (vOut + vMint)

ledgerTypedValidator :: TypedValidator Ledgering
ledgerTypedValidator = mkTypedValidator @Ledgering
    $$(PlutusTx.compile [|| ledgerValidatorCheck ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @() @()
