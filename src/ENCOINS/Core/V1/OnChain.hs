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

-- StakeOwner reference, Beacon reference, verifierPKH
type EncoinsProtocolParams = (TxOutRef, TxOutRef, BuiltinByteString)

---------------------------- Stake Owner Token Minting Policy --------------------------------------

{-# INLINABLE stakeOwnerTokenName #-}
stakeOwnerTokenName :: TokenName
stakeOwnerTokenName = TokenName emptyByteString

{-# INLINABLE stakeOwnerMintParams #-}
stakeOwnerMintParams :: EncoinsProtocolParams -> OneShotCurrencyParams
stakeOwnerMintParams (ref, _, _) = mkCurrency ref [(stakeOwnerTokenName, 1)]

stakeOwnerPolicy :: EncoinsProtocolParams -> MintingPolicy
stakeOwnerPolicy = oneShotCurrencyPolicy . stakeOwnerMintParams

stakeOwnerPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
stakeOwnerPolicyV = flip Versioned PlutusV2 . stakeOwnerPolicy

stakeOwnerCurrencySymbol :: EncoinsProtocolParams -> CurrencySymbol
stakeOwnerCurrencySymbol = scriptCurrencySymbol . stakeOwnerPolicy

stakeOwnerAssetClass :: EncoinsProtocolParams -> AssetClass
stakeOwnerAssetClass ref = AssetClass (stakeOwnerCurrencySymbol ref, stakeOwnerTokenName)

stakeOwnerToken :: EncoinsProtocolParams -> Value
stakeOwnerToken = token . stakeOwnerAssetClass

-------------------------------------- Beacon Minting Policy ---------------------------------------

{-# INLINABLE beaconTokenName #-}
beaconTokenName :: TokenName
beaconTokenName = TokenName emptyByteString

{-# INLINABLE beaconMintParams #-}
beaconMintParams :: EncoinsProtocolParams -> OneShotCurrencyParams
beaconMintParams (_, ref, _) = mkCurrency ref [(beaconTokenName, 1)]

beaconPolicy :: EncoinsProtocolParams -> MintingPolicy
beaconPolicy = oneShotCurrencyPolicy . beaconMintParams

beaconPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
beaconPolicyV = flip Versioned PlutusV2 . beaconPolicy

beaconCurrencySymbol :: EncoinsProtocolParams -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconPolicy

beaconAssetClass :: EncoinsProtocolParams -> AssetClass
beaconAssetClass par = AssetClass (beaconCurrencySymbol par, beaconTokenName)

beaconToken :: EncoinsProtocolParams -> Value
beaconToken = token . beaconAssetClass

----------------------------------- ENCOINS Minting Policy ---------------------------------------

-- Beacon token and verifierPKH
type EncoinsPolicyParams = (Value, BuiltinByteString)

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
encoinsPolicyCheck :: EncoinsPolicyParams -> EncoinsRedeemer -> ScriptContext -> Bool
encoinsPolicyCheck (beacon, verifierPKH) red@((ledgerAddr, changeAddr), (v, inputs), _, sig)
    ctx@ScriptContext{scriptContextTxInfo=info} =
      cond0
      && cond1
      && cond2
      && cond3
      && (cond4 || cond5)
  where
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

toEncoinsPolicyParams :: EncoinsProtocolParams -> EncoinsPolicyParams
toEncoinsPolicyParams par@(_, _, verifierPKH) = (beaconToken par, verifierPKH)

encoinsPolicy :: EncoinsProtocolParams -> MintingPolicy
encoinsPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . encoinsPolicyCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode (toEncoinsPolicyParams par)

encoinsPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
encoinsPolicyV = flip Versioned PlutusV2 . encoinsPolicy

encoinsSymbol :: EncoinsProtocolParams -> CurrencySymbol
encoinsSymbol = scriptCurrencySymbol . encoinsPolicy

encoinsAssetClass :: EncoinsProtocolParams -> BuiltinByteString -> AssetClass
encoinsAssetClass par a = AssetClass (encoinsSymbol par, encoinName a)

encoin :: EncoinsProtocolParams -> BuiltinByteString -> Value
encoin par = token . encoinsAssetClass par

encoinsInValue :: EncoinsProtocolParams -> Value -> [BuiltinByteString]
encoinsInValue par = map unTokenName . maybe [] keys . lookup (encoinsSymbol par) . getValue

--------------------------------------- ENCOINS Stake Validator ----------------------------------------

-- Stake owner token
type EncoinsStakeValidatorParams = Value

{-# INLINABLE encoinsStakeValidatorCheck #-}
encoinsStakeValidatorCheck :: EncoinsStakeValidatorParams -> () -> ScriptContext -> Bool
encoinsStakeValidatorCheck stakeOwner _ ScriptContext{scriptContextTxInfo=info} =
    utxoSpent info (\o -> txOutValue o `geq` stakeOwner)

encoinsStakeValidator :: EncoinsProtocolParams -> StakeValidator
encoinsStakeValidator par = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkUntypedStakeValidator . encoinsStakeValidatorCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode (stakeOwnerToken par)

encoinsStakeValidatorV :: EncoinsProtocolParams -> Versioned StakeValidator
encoinsStakeValidatorV = flip Versioned PlutusV2 . encoinsStakeValidator

encoinsStakeValidatorHash :: EncoinsProtocolParams -> StakeValidatorHash
encoinsStakeValidatorHash = stakeValidatorHash . encoinsStakeValidator

------------------------------------- ENCOINS Ledger Validator --------------------------------------

-- ENCOINS currency symbol
type EncoinsLedgerValidatorParams = CurrencySymbol

{-# INLINABLE ledgerValidatorCheck #-}
ledgerValidatorCheck :: EncoinsLedgerValidatorParams -> () -> () -> ScriptContext -> Bool
ledgerValidatorCheck encoinsSymb _ _
    ScriptContext{scriptContextTxInfo=info} =  Minting encoinsSymb `member` txInfoRedeemers info

ledgerValidator :: EncoinsProtocolParams -> Validator
ledgerValidator par = mkValidatorScript $
    $$(PlutusTx.compile [|| mkUntypedValidator . ledgerValidatorCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode (encoinsSymbol par)

ledgerValidatorV :: EncoinsProtocolParams -> Versioned Validator
ledgerValidatorV = flip Versioned PlutusV2 . ledgerValidator

ledgerValidatorHash :: EncoinsProtocolParams -> ValidatorHash
ledgerValidatorHash = validatorHash . ledgerValidator

ledgerValidatorAddress :: EncoinsProtocolParams -> Address
ledgerValidatorAddress par =
    let StakeValidatorHash vh = encoinsStakeValidatorHash par
    in Address
    (ScriptCredential (ledgerValidatorHash par))
    (Just $ StakingHash $ ScriptCredential $ ValidatorHash vh)
        