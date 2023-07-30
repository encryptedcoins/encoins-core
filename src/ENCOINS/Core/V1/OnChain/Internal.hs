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

module ENCOINS.Core.V1.OnChain.Internal where

import           Ledger.Ada                                (lovelaceValueOf)
import           Ledger.Tokens                             (token)
import           Ledger.Typed.Scripts                      (IsScriptContext(..), Versioned (..), Language (..))
import           Ledger.Value                              (AssetClass (..), geq, flattenValue, valueOf)
import           Plutus.Script.Utils.V2.Scripts            (scriptCurrencySymbol, stakeValidatorHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx                                  (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                         (member)
import           PlutusTx.Prelude

import           ENCOINS.Bulletproofs                      (Proof)
import           ENCOINS.BaseTypes                         (MintingPolarity)
import           ENCOINS.Orphans                           ()
import           PlutusAppsExtra.Constraints.OnChain       (utxoSpent)
import           PlutusAppsExtra.Scripts.OneShotCurrency   (OneShotCurrencyParams, mkCurrency, oneShotCurrencyPolicy)
import           PlutusAppsExtra.Utils.Orphans             ()

-- StakeOwner reference, Beacon reference, verifierPKH
type EncoinsProtocolParams = (TxOutRef, TxOutRef, BuiltinByteString)

minAdaTxOutInLedger :: Integer
minAdaTxOutInLedger = 2_000_000

minTxOutValueInLedger :: Value
minTxOutValueInLedger = lovelaceValueOf minAdaTxOutInLedger

minMaxAdaTxOutInLedger :: Integer
minMaxAdaTxOutInLedger = 1000_000_000

minMaxTxOutValueInLedger :: Value
minMaxTxOutValueInLedger = lovelaceValueOf minMaxAdaTxOutInLedger

depositMultiplier :: Integer
depositMultiplier = 2

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
type EncoinsInputOnChain = (Integer, [(TokenName, Integer)])
type ProofHash = BuiltinByteString
type ProofSignature = BuiltinByteString
type EncoinsRedeemer = (TxParams, EncoinsInput, Proof, ProofSignature)
type EncoinsRedeemerOnChain = (TxParams, EncoinsInputOnChain, ProofHash, ProofSignature)

{-# INLINABLE inputToBytes #-}
inputToBytes :: (TokenName, Integer) -> BuiltinByteString
inputToBytes (TokenName bs, i) = bs `appendByteString` consByteString (if i == 1 then 1 else 0) emptyByteString

{-# INLINABLE encoinName #-}
encoinName :: BuiltinByteString -> TokenName
encoinName = TokenName

{-# INLINABLE checkLedgerOutputValue1 #-}
checkLedgerOutputValue1 :: [Value] -> Bool
checkLedgerOutputValue1 [] = True
checkLedgerOutputValue1 (v:vs) = length (flattenValue v) <= 2 && checkLedgerOutputValue2 vs

{-# INLINABLE checkLedgerOutputValue2 #-}
checkLedgerOutputValue2 :: [Value] -> Bool
checkLedgerOutputValue2 [] = True
checkLedgerOutputValue2 (v:vs) = length (flattenValue v) == 2 && valueOf v adaSymbol adaToken == minAdaTxOutInLedger && checkLedgerOutputValue2 vs

toEncoinsPolicyParams :: EncoinsProtocolParams -> EncoinsPolicyParams
toEncoinsPolicyParams par@(_, _, verifierPKH) = (beaconToken par, verifierPKH)

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