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

module ENCOINS.Core.V1.OnChain.Plutus where

import           Data.Maybe                                (fromJust)
import           Ledger.Ada                                (lovelaceValueOf, getLovelace, fromValue)
import           Ledger.Tokens                             (token)
import           Ledger.Typed.Scripts                      (IsScriptContext(..), Versioned (..), Language (..))
import           Ledger.Value                              (AssetClass (..), geq, flattenValue)
import           Plutus.Script.Utils.V2.Scripts            (validatorHash, scriptCurrencySymbol, stakeValidatorHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx                                  (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                         (lookup, keys, member)
import           PlutusTx.Prelude
import           Text.Hex                                  (decodeHex)

import           ENCOINS.Bulletproofs                      (Proof, polarityToInteger)
import           ENCOINS.BaseTypes                         (MintingPolarity)
import           ENCOINS.Orphans                           ()
import           PlutusAppsExtra.Constraints.OnChain       (tokensMinted, filterUtxoSpent, utxoReferenced, utxoProduced, utxoSpent, filterUtxoProduced)
import           PlutusAppsExtra.Scripts.OneShotCurrency   (OneShotCurrencyParams, mkCurrency, oneShotCurrencyPolicy)
import           PlutusAppsExtra.Utils.Datum
import           PlutusAppsExtra.Utils.Orphans             ()
import           PlutusTx.Extra.ByteString                 (ToBuiltinByteString(..))

-- StakeOwner reference, Beacon reference, verifierPKH
type EncoinsProtocolParams = (TxOutRef, TxOutRef, BuiltinByteString)

minAdaTxOutInLedger :: Integer
minAdaTxOutInLedger = 2_000_000

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

-- Ledger address, change addresses, total fees
type TxParams = (Address, Address, Integer)
type EncoinsInput = (Integer, [(BuiltinByteString, MintingPolarity)])
type ProofHash = BuiltinByteString
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)
type EncoinsRedeemerOnChain = (TxParams, EncoinsInput, ProofHash, ProofSignature)

hashRedeemer :: EncoinsRedeemerOnChain -> BuiltinByteString
hashRedeemer ((_, changeAddr, fees), (v, inputs), proofHash, _) =
    sha2_256 $ toBytes changeAddr `appendByteString` toBytes fees `appendByteString` toBytes (v, inputs) `appendByteString` proofHash

{-# INLINABLE encoinName #-}
encoinName :: BuiltinByteString -> TokenName
encoinName = TokenName

-- TODO: remove on-chain sorting (requires sorting inputs and proof components)
encoinsPolicyCheck :: EncoinsPolicyParams -> EncoinsRedeemerOnChain -> ScriptContext -> Bool
encoinsPolicyCheck (beacon, verifierPKH) red@((ledgerAddr, changeAddr, fees), (v, inputs), _, sig)
    ctx@ScriptContext{scriptContextTxInfo=info} =
      cond0
      && cond1
      && cond2
      && cond3
      && (cond4 || cond5)
      && cond6
      && cond7
  where
      fees'   = abs fees
      val     = lovelaceValueOf (v * 1_000_000)
      valFees = lovelaceValueOf (fees' * 1_000_000)

      cond0 = tokensMinted ctx $ fromList $ sort $ map (\(bs, p) -> (encoinName bs, polarityToInteger p)) inputs
      cond1 = verifyEd25519Signature verifierPKH (hashRedeemer red) sig
      cond2 = (v + fees' >= 0) || utxoProduced info (\o -> txOutAddress o == changeAddr && txOutValue o `geq` (zero - val - valFees))
      cond3 = utxoReferenced info (\o -> txOutAddress o == ledgerAddr && txOutValue o `geq` beacon)

      vMint = txInfoMint $ scriptContextTxInfo ctx
      vOuts = map txOutValue $ filterUtxoSpent info (\o -> txOutAddress o == ledgerAddr)
      vOut  = sum vOuts
      vIns  = map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == ledgerAddr && isInlineUnit (txOutDatum o))
      vIn   = sum vIns

      cond4 = vIn == (vOut + val)         -- Wallet Mode
      cond5 = vIn == (vOut + vMint + val) -- Ledger Mode

      -- TxOuts size limit
      txOutSize = sort $ map (length . flattenValue) vIns
      cond6 = null vIns || (all (6 ==) (tail txOutSize) && (head txOutSize <= 6))

      -- ADA value is concentrated in a single TxOut
      adaVals   = sortBy (flip compare) $ map (getLovelace . fromValue) vIns
      cond7 = null vIns || all (minAdaTxOutInLedger ==) (tail adaVals)

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
ledgerValidatorAddress par = Address
    (ScriptCredential (ledgerValidatorHash par))
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $
        toBuiltin $ fromJust $ decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")

-- TODO: implement stake validator off-chain logic
-- ledgerValidatorAddress :: EncoinsProtocolParams -> Address
-- ledgerValidatorAddress par =
--     let StakeValidatorHash vh = encoinsStakeValidatorHash par
--     in Address
--     (ScriptCredential (ledgerValidatorHash par))    
--     (Just $ StakingHash $ ScriptCredential $ ValidatorHash vh)
