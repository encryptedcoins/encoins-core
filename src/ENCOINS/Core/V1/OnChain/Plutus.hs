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

import           Data.Maybe                          (fromJust)
import           Ledger.Ada                          (lovelaceValueOf)
import           Ledger.Tokens                       (token)
import           Ledger.Typed.Scripts                (IsScriptContext (..), Language (..), Versioned (..))
import           Ledger.Value                        (AssetClass (..), geq)
import           Plutus.Script.Utils.V2.Scripts      (scriptCurrencySymbol, validatorHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx                            (applyCode, compile, liftCode)
import           PlutusTx.AssocMap                   (keys, lookup)
import           PlutusTx.Extra.ByteString           (toBytes)
import           PlutusTx.Prelude
import           Text.Hex                            (decodeHex)

import           ENCOINS.Core.V1.OnChain.Internal    (EncoinsPolicyParams, EncoinsProtocolParams, EncoinsRedeemerOnChain,
                                                      checkLedgerOutputValue1, depositMultiplier, encoinName, inputToBytes,
                                                      ledgerValidatorCheck, minTxOutValueInLedger, toEncoinsPolicyParams)
import           ENCOINS.Orphans                     ()
import           PlutusAppsExtra.Constraints.OnChain (filterUtxoProduced, filterUtxoSpent, tokensMinted, utxoProduced,
                                                      utxoReferenced)
import           PlutusAppsExtra.Utils.Datum         (isInlineUnit)
import           PlutusAppsExtra.Utils.Orphans       ()

-- ----------------------------------- ENCOINS Minting Policy ---------------------------------------

{-# INLINABLE hashRedeemer #-}
hashRedeemer :: EncoinsRedeemerOnChain -> BuiltinByteString
hashRedeemer ((_, changeAddr, fees), (v, inputs), proofHash, _) =
    sha2_256 $ toBytes changeAddr `appendByteString` toBytes fees `appendByteString` toBytes v
    `appendByteString` foldr (appendByteString . inputToBytes) emptyByteString inputs `appendByteString` proofHash

{-# INLINABLE encoinsPolicyCheck #-}
encoinsPolicyCheck :: EncoinsPolicyParams -> EncoinsRedeemerOnChain -> ScriptContext -> Bool
encoinsPolicyCheck (beacon, verifierPKH) red@((ledgerAddr, changeAddr, fees), (v, inputs), _, sig)
    ctx@ScriptContext{scriptContextTxInfo=info} =
      cond0
      && cond1
      && cond2
      && cond3
      && (cond4 || cond5)
      && cond6
  where
      val          = lovelaceValueOf (v * 1_000_000)

      fees'        = abs fees
      valFees      = lovelaceValueOf (fees' * 1_000_000)

      deposits     = depositMultiplier * sum (map snd inputs)
      valDeposits  = lovelaceValueOf (deposits * 1_000_000)

      deposits'    = if cond5 then deposits else 0
      valDeposits' = lovelaceValueOf (deposits' * 1_000_000)

      cond0 = tokensMinted ctx $ fromList inputs
      cond1 = verifyEd25519Signature verifierPKH (hashRedeemer red) sig
      cond2 = (v + fees' + deposits' >= 0) || utxoProduced info (\o -> txOutAddress o == changeAddr && txOutValue o `geq` (zero - val - valFees - valDeposits'))
      cond3 = utxoReferenced info (\o -> txOutAddress o == ledgerAddr && txOutValue o `geq` beacon)

      vMint = txInfoMint $ scriptContextTxInfo ctx
      vOuts = map txOutValue $ filterUtxoSpent info (\o -> txOutAddress o == ledgerAddr && txOutValue o `geq` minTxOutValueInLedger)
      vOut  = sum vOuts
      vIns  = map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == ledgerAddr && txOutValue o `geq` minTxOutValueInLedger && isInlineUnit (txOutDatum o))
      vIn   = sum vIns

      cond4 = vIn == (vOut + val)                       -- Wallet Mode
      cond5 = vIn == (vOut + vMint + val + valDeposits) -- Ledger Mode

      -- The ENCOINS Ledger output values (only two are allowed) must satisfy conditions on the size and ADA concentration
      cond6 = checkLedgerOutputValue1 vIns

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

------------------------------------- ENCOINS Ledger Validator --------------------------------------

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

-- -- TODO: implement stake validator off-chain logic
-- -- ledgerValidatorAddress :: EncoinsProtocolParams -> Address
-- -- ledgerValidatorAddress par =
-- --     let StakeValidatorHash vh = encoinsStakeValidatorHash par
-- --     in Address
-- --     (ScriptCredential (ledgerValidatorHash par))    
-- --     (Just $ StakingHash $ ScriptCredential $ ValidatorHash vh)
