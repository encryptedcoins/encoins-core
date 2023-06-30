{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.V1.OnChain.Aiken where

import           Data.Maybe                         (fromJust)
import           Ledger.Tokens                      (token)
import           Ledger.Typed.Scripts               (Versioned (..), Language (..))
import           Ledger.Value                       (AssetClass (..))
import           Plutus.Script.Utils.V2.Scripts     (validatorHash, scriptCurrencySymbol)
import           Plutus.V2.Ledger.Api
import           PlutusTx.AssocMap                  (lookup, keys)
import           PlutusTx.Prelude
import           Text.Hex                           (decodeHex)

import           ENCOINS.Core.V1.OnChain.Plutus     (EncoinsRedeemerOnChain, EncoinsProtocolParams, encoinName, toEncoinsPolicyParams)
import           ENCOINS.Core.V1.OnChain.Aiken.UPLC (ledgerValidatorCheck, encoinsPolicyCheck)
import           PlutusAppsExtra.Utils.Scripts      (unsafeParameterizedValidatorFromCBOR, unsafeParameterizedMintingPolicyFromCBOR)
import           PlutusTx.Builtins                  (serialiseData)

----------------------------------- ENCOINS Minting Policy ---------------------------------------

hashRedeemer :: EncoinsRedeemerOnChain -> BuiltinByteString
hashRedeemer (a, b, c, _) = sha2_256 . serialiseData . toBuiltinData $ (a, b, c)

encoinsPolicy :: EncoinsProtocolParams -> MintingPolicy
encoinsPolicy = unsafeParameterizedMintingPolicyFromCBOR encoinsPolicyCheck . toEncoinsPolicyParams

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
ledgerValidator = unsafeParameterizedValidatorFromCBOR ledgerValidatorCheck . encoinsSymbol

ledgerValidatorV :: EncoinsProtocolParams -> Versioned Validator
ledgerValidatorV = flip Versioned PlutusV2 . ledgerValidator

ledgerValidatorHash :: EncoinsProtocolParams -> ValidatorHash
ledgerValidatorHash = validatorHash . ledgerValidator

ledgerValidatorAddress :: EncoinsProtocolParams -> Address
ledgerValidatorAddress par = Address
    (ScriptCredential (ledgerValidatorHash par))
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $
        toBuiltin $ fromJust $ decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")