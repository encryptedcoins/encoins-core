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

import           Ledger.Ada                                (lovelaceValueOf)
import           Ledger.Tokens                             (token)
import           Ledger.Typed.Scripts                      (IsScriptContext(..), Versioned (..), Language (..))
import           Ledger.Value                              (AssetClass (..), geq)
import           Plutus.Script.Utils.V2.Scripts            (validatorHash, scriptCurrencySymbol, stakeValidatorHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx                                  (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                         (lookup, keys, member)
import           PlutusTx.Prelude

import           ENCOINS.Bulletproofs                      (Proof, polarityToInteger)
import           ENCOINS.BaseTypes                         (MintingPolarity)
import           ENCOINS.Orphans                           ()
import           PlutusAppsExtra.Constraints.OnChain       (tokensMinted, filterUtxoSpent, utxoReferenced, utxoProduced, findUtxoProduced, utxoSpent)
import           PlutusAppsExtra.Scripts.OneShotCurrency   (OneShotCurrencyParams, mkCurrency, oneShotCurrencyPolicy)
import           PlutusAppsExtra.Utils.Datum
import           PlutusAppsExtra.Utils.Orphans             ()
import           PlutusTx.Extra.ByteString                 (ToBuiltinByteString(..))

---------------------------- Stake Owner Token Minting Policy --------------------------------------

{-# INLINABLE stakeOwnerTokenName #-}
stakeOwnerTokenName :: TokenName
stakeOwnerTokenName = TokenName emptyByteString

{-# INLINABLE stakeOwnerParams #-}
stakeOwnerParams :: TxOutRef -> OneShotCurrencyParams
stakeOwnerParams ref = mkCurrency ref [(stakeOwnerTokenName, 1)]

stakeOwnerPolicy :: TxOutRef -> MintingPolicy
stakeOwnerPolicy = oneShotCurrencyPolicy . stakeOwnerParams

stakeOwnerPolicyV :: TxOutRef -> Versioned MintingPolicy
stakeOwnerPolicyV = flip Versioned PlutusV2 . stakeOwnerPolicy

stakeOwnerCurrencySymbol :: TxOutRef -> CurrencySymbol
stakeOwnerCurrencySymbol = scriptCurrencySymbol . stakeOwnerPolicy

stakeOwnerAssetClass :: TxOutRef -> AssetClass
stakeOwnerAssetClass ref = AssetClass (stakeOwnerCurrencySymbol ref, stakeOwnerTokenName)

stakeOwnerToken :: TxOutRef -> Value
stakeOwnerToken = token . stakeOwnerAssetClass

-------------------------------------- Beacon Minting Policy ---------------------------------------

{-# INLINABLE beaconTokenName #-}
beaconTokenName :: TokenName
beaconTokenName = TokenName emptyByteString

{-# INLINABLE beaconParams #-}
beaconParams :: TxOutRef -> OneShotCurrencyParams
beaconParams ref = mkCurrency ref [(beaconTokenName, 1)]

beaconPolicy :: TxOutRef -> MintingPolicy
beaconPolicy = oneShotCurrencyPolicy . beaconParams

beaconPolicyV :: TxOutRef -> Versioned MintingPolicy
beaconPolicyV = flip Versioned PlutusV2 . beaconPolicy

beaconCurrencySymbol :: TxOutRef -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconPolicy

beaconAssetClass :: TxOutRef -> AssetClass
beaconAssetClass ref = AssetClass (beaconCurrencySymbol ref, beaconTokenName)

beaconToken :: TxOutRef -> Value
beaconToken = token . beaconAssetClass

----------------------------------- ENCOINS Minting Policy ---------------------------------------

-- Beacon currency symbol and verifierPKH
type EncoinsParams = (CurrencySymbol, BuiltinByteString)

-- Ledger and change addresses
type TxParams = (Address, Address)
type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)

hashRedeemer :: EncoinsRedeemer -> BuiltinByteString
hashRedeemer ((ledgerAddr, changeAddr), (v, inputs), proof, _) =
    sha2_256 $ toBytes ledgerAddr `appendByteString` toBytes changeAddr `appendByteString` toBytes (v, inputs) `appendByteString` toBytes proof

{-# INLINABLE encoinName #-}
encoinName :: BuiltinByteString -> TokenName
encoinName = TokenName

-- TODO: remove on-chain sorting (requires sorting inputs and proof components)
-- TODO: add constraints on the tokens in the produced Ledger utxo
encoinsPolicyCheck :: EncoinsParams -> EncoinsRedeemer -> ScriptContext -> Bool
encoinsPolicyCheck par@(beaconSymb, verifierPKH) red@((ledgerAddr, changeAddr), (v, inputs), _, sig)
    ctx@ScriptContext{scriptContextTxInfo=info} =
      cond0
      && cond1
      && cond2
      && cond3
      && (cond4 || cond5)
      && cond6
      && cond7
  where
      beacon = token (AssetClass (beaconSymb, beaconTokenName))
      val   = lovelaceValueOf (v * 1_000_000)

      cond0 = tokensMinted ctx $ fromList $ sort $ map (\(bs, p) -> (encoinName bs, polarityToInteger p)) inputs
      cond1 = verifyEd25519Signature verifierPKH (hashRedeemer red) sig
      cond2 = utxoProduced info (\o -> txOutAddress o == changeAddr && txOutValue o `geq` (zero-val))
      cond3 = utxoReferenced info (\o -> txOutAddress o == ledgerAddr && txOutValue o `geq` beacon)

      vMint = txInfoMint $ scriptContextTxInfo ctx
      vOut  = sum $ map txOutValue $ filterUtxoSpent info (\o -> txOutAddress o == ledgerAddr)
      vIn   = maybe zero txOutValue $ findUtxoProduced info (\o -> txOutAddress o == ledgerAddr && isInlineUnit (txOutDatum o))

      cond4 = vIn == (vOut + val)         -- Wallet Mode
      cond5 = vIn == (vOut + vMint + val) -- Ledger Mode

      cond6 = keys (getValue vIn) == [adaSymbol, encoinsSymbol par]
      cond7 = length (encoinsInValue par vIn) < 6

encoinsPolicy :: EncoinsParams -> MintingPolicy
encoinsPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . encoinsPolicyCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

encoinsPolicyV :: EncoinsParams -> Versioned MintingPolicy
encoinsPolicyV = flip Versioned PlutusV2 . encoinsPolicy

encoinsSymbol :: EncoinsParams -> CurrencySymbol
encoinsSymbol = scriptCurrencySymbol . encoinsPolicy

encoinsAssetClass :: EncoinsParams -> BuiltinByteString -> AssetClass
encoinsAssetClass par a = AssetClass (encoinsSymbol par, encoinName a)

encoin :: EncoinsParams -> BuiltinByteString -> Value
encoin par = token . encoinsAssetClass par

{-# INLINABLE encoinsInValue #-}
encoinsInValue :: EncoinsParams -> Value -> [BuiltinByteString]
encoinsInValue par = map unTokenName . maybe [] keys . lookup (encoinsSymbol par) . getValue

--------------------------------------- ENCOINS Stake Validator ----------------------------------------

-- Stake owner currency symbol
type EncoinsStakeParams = CurrencySymbol

{-# INLINABLE encoinsStakeValidatorCheck #-}
encoinsStakeValidatorCheck :: EncoinsStakeParams -> () -> ScriptContext -> Bool
encoinsStakeValidatorCheck stakeOwnerSymb _ ScriptContext{scriptContextTxInfo=info} = cond0
  where
    stakeOwner = token (AssetClass (stakeOwnerSymb, stakeOwnerTokenName))

    cond0 = utxoSpent info (\o -> txOutValue o `geq` stakeOwner)

encoinsStakeValidator :: EncoinsStakeParams -> StakeValidator
encoinsStakeValidator par = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkUntypedStakeValidator . encoinsStakeValidatorCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

encoinsStakeValidatorV :: EncoinsStakeParams -> Versioned StakeValidator
encoinsStakeValidatorV = flip Versioned PlutusV2 . encoinsStakeValidator

encoinsStakeValidatorHash :: EncoinsStakeParams -> StakeValidatorHash
encoinsStakeValidatorHash = stakeValidatorHash . encoinsStakeValidator

------------------------------------- ENCOINS Ledger Validator --------------------------------------

-- ENCOINS currency symbol
type EncoinsLedgerParams = CurrencySymbol

{-# INLINABLE ledgerValidatorCheck #-}
ledgerValidatorCheck :: EncoinsLedgerParams -> () -> () -> ScriptContext -> Bool
ledgerValidatorCheck encoinsSymb _ _
    ScriptContext{scriptContextTxInfo=info} =  Minting encoinsSymb `member` txInfoRedeemers info

ledgerValidator :: EncoinsLedgerParams -> Validator
ledgerValidator par = mkValidatorScript $
    $$(PlutusTx.compile [|| mkUntypedValidator . ledgerValidatorCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

ledgerValidatorV :: EncoinsLedgerParams -> Versioned Validator
ledgerValidatorV = flip Versioned PlutusV2 . ledgerValidator

ledgerValidatorHash :: EncoinsLedgerParams -> ValidatorHash
ledgerValidatorHash = validatorHash . ledgerValidator

type EncoinsSpendParams = (EncoinsLedgerParams, EncoinsStakeParams)

ledgerValidatorAddress :: EncoinsSpendParams -> Address
ledgerValidatorAddress (encoinsSymb, stakeOwnerSymb) = Address
    (ScriptCredential (ledgerValidatorHash encoinsSymb))
    (Just $ StakingHash $ ScriptCredential $ ValidatorHash vh)
    where StakeValidatorHash vh = encoinsStakeValidatorHash stakeOwnerSymb