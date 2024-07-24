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

import           Ledger.Typed.Scripts                           (IsScriptContext (..), Language (..), Versioned (..))
import           Plutus.Script.Utils.V3.Scripts                 (MintingPolicy, StakeValidator, StakeValidatorHash, scriptCurrencySymbol,
                                                                 stakeValidatorHash)
import           PlutusLedgerApi.V2                             (BuiltinByteString, TxOut (txOutValue), TxOutRef, Value)
import           PlutusTx                                       (compile, liftCode, unsafeApplyCode)
import           PlutusTx.AssocMap                              (member)
import           PlutusTx.Prelude                               (Bool (..), Eq (..), Integer, Ord (..), appendByteString, consByteString,
                                                                 emptyByteString, length, ($), (&&), (.))

import           ENCOINS.BaseTypes                              (MintingPolarity)
import           ENCOINS.Bulletproofs                           (Proof)
import           ENCOINS.Orphans                                ()
import           Ledger                                         (Address, mkStakeValidatorScript)
import qualified Plutus.Script.Utils.Ada                        as P
import           Plutus.Script.Utils.V3.Contexts                (ScriptContext (..), ScriptPurpose (..), TxInfo (..))
import           Plutus.Script.Utils.Value                      (AssetClass (..), CurrencySymbol, TokenName (..), adaSymbol, adaToken,
                                                                 assetClassValue, geq)
import qualified Plutus.Script.Utils.Value                      as P
import           PlutusAppsExtra.Constraints.OnChain            (utxoSpent)
import           PlutusAppsExtra.Scripts.Legacy.OneShotCurrency (LegacyOneShotCurrencyParams (..), legacyOneShotCurrencyPolicy)
import           PlutusAppsExtra.Utils.Orphans                  ()
import           PlutusCore.Core                                (plcVersion100)

-- StakeOwner reference, Beacon reference, verifierPKH, validator stake key
type EncoinsProtocolParams = (TxOutRef, TxOutRef, BuiltinByteString, BuiltinByteString)

minAdaTxOutInLedger :: Integer
minAdaTxOutInLedger = 4_000_000

minTxOutValueInLedger :: P.Value
minTxOutValueInLedger = P.lovelaceValueOf minAdaTxOutInLedger

minMaxAdaTxOutInLedger :: Integer
minMaxAdaTxOutInLedger = 1000_000_000

minMaxTxOutValueInLedger :: Value
minMaxTxOutValueInLedger = P.lovelaceValueOf minMaxAdaTxOutInLedger

---------------------------- Stake Owner Token Minting Policy --------------------------------------

{-# INLINABLE stakeOwnerTokenName #-}
stakeOwnerTokenName :: TokenName
stakeOwnerTokenName = TokenName emptyByteString

{-# INLINABLE stakeOwnerMintParams #-}
stakeOwnerMintParams :: EncoinsProtocolParams -> LegacyOneShotCurrencyParams
stakeOwnerMintParams (ref, _, _, _) = LegacyOneShotCurrencyParams ref stakeOwnerTokenName 1

stakeOwnerPolicy :: EncoinsProtocolParams -> MintingPolicy
stakeOwnerPolicy = legacyOneShotCurrencyPolicy . stakeOwnerMintParams

stakeOwnerPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
stakeOwnerPolicyV = (`Versioned` PlutusV2) . stakeOwnerPolicy

stakeOwnerCurrencySymbol :: EncoinsProtocolParams -> CurrencySymbol
stakeOwnerCurrencySymbol = scriptCurrencySymbol . stakeOwnerPolicy

stakeOwnerAssetClass :: EncoinsProtocolParams -> AssetClass
stakeOwnerAssetClass par = AssetClass (stakeOwnerCurrencySymbol par, stakeOwnerTokenName)

stakeOwnerToken :: EncoinsProtocolParams -> Value
stakeOwnerToken par = (`assetClassValue` 1) $ stakeOwnerAssetClass par

-------------------------------------- Beacon Minting Policy ---------------------------------------

{-# INLINABLE beaconTokenName #-}
beaconTokenName :: TokenName
beaconTokenName = TokenName emptyByteString

{-# INLINABLE beaconMintParams #-}
beaconMintParams :: EncoinsProtocolParams -> LegacyOneShotCurrencyParams
beaconMintParams (_, ref, _, _) = LegacyOneShotCurrencyParams ref beaconTokenName 1

beaconPolicy :: EncoinsProtocolParams -> MintingPolicy
beaconPolicy = legacyOneShotCurrencyPolicy . beaconMintParams

beaconPolicyV :: EncoinsProtocolParams -> Versioned MintingPolicy
beaconPolicyV = (`Versioned` PlutusV2) . beaconPolicy

beaconCurrencySymbol :: EncoinsProtocolParams -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconPolicy

beaconAssetClass :: EncoinsProtocolParams -> AssetClass
beaconAssetClass par = AssetClass (beaconCurrencySymbol par, beaconTokenName)

beaconToken :: EncoinsProtocolParams -> Value
beaconToken = (`assetClassValue` 1) . beaconAssetClass

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
checkLedgerOutputValue1 []     = True
checkLedgerOutputValue1 (v:vs) = length (P.flattenValue v) <= 2 && checkLedgerOutputValue2 vs

{-# INLINABLE checkLedgerOutputValue2 #-}
checkLedgerOutputValue2 :: [Value] -> Bool
checkLedgerOutputValue2 [] = True
checkLedgerOutputValue2 (v:vs) = length (P.flattenValue v) == 2 && P.valueOf v adaSymbol adaToken == minAdaTxOutInLedger && checkLedgerOutputValue2 vs

toEncoinsPolicyParams :: EncoinsProtocolParams -> EncoinsPolicyParams
toEncoinsPolicyParams par@(_, _, verifierPKH, _) = (beaconToken par, verifierPKH)

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
        `PlutusTx.unsafeApplyCode`
                PlutusTx.liftCode plcVersion100 (stakeOwnerToken par)

encoinsStakeValidatorV :: EncoinsProtocolParams -> Versioned StakeValidator
encoinsStakeValidatorV = (`Versioned` PlutusV2) . encoinsStakeValidator

encoinsStakeValidatorHash :: EncoinsProtocolParams -> StakeValidatorHash
encoinsStakeValidatorHash = stakeValidatorHash . encoinsStakeValidator

------------------------------------- ENCOINS Ledger Validator --------------------------------------

-- ENCOINS currency symbol
type EncoinsLedgerValidatorParams = CurrencySymbol

{-# INLINABLE ledgerValidatorCheck #-}
ledgerValidatorCheck :: EncoinsLedgerValidatorParams -> () -> () -> ScriptContext -> Bool
ledgerValidatorCheck encoinsSymb _ _
    ScriptContext{scriptContextTxInfo=info} =  Minting encoinsSymb `member` txInfoRedeemers info
