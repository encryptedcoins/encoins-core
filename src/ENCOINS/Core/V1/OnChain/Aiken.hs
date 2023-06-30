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
import           Ledger.Typed.Scripts               (Language (..), Versioned (..))
import           Ledger.Value                       (AssetClass (..))
import           Plutus.Script.Utils.V2.Scripts     (scriptCurrencySymbol, validatorHash)
import           Plutus.V2.Ledger.Api
import           PlutusTx.AssocMap                  (keys, lookup)
import           PlutusTx.Prelude
import           Text.Hex                           (decodeHex)

import           Data.Bifunctor                     (Bifunctor (..))
import           ENCOINS.Core.V1.OnChain.Aiken.UPLC (encoinsPolicyCheck, ledgerValidatorCheck)
import           ENCOINS.Core.V1.OnChain.Internal   (EncoinsLedgerValidatorParams, EncoinsPolicyParams, EncoinsProtocolParams,
                                                     EncoinsRedeemerOnChain, encoinName, toEncoinsPolicyParams, TxParams, ProofHash, EncoinsInputOnChain)
import           PlutusAppsExtra.Utils.Scripts      (unsafeParameterizedMintingPolicyFromCBOR,
                                                     unsafeParameterizedValidatorFromCBOR)
import           PlutusTx.Builtins                  (serialiseData)

-------------------------------------- ToData instances --------------------------------------

newtype Aiken a = Aiken a

instance ToData (Aiken EncoinsLedgerValidatorParams) where
    toBuiltinData (Aiken cs) = BuiltinData $ Constr 121 [toData cs]

instance ToData (Aiken EncoinsPolicyParams) where
    toBuiltinData (Aiken (val, bbs)) = BuiltinData $ List [toData val, toData bbs]

-- instance ToData (Aiken EncoinsInput) where
--     toBuiltinData (Aiken (iVal, inputs)) = BuiltinData $ List [I iVal, Map (bimap toData (I . polarityToInteger) <$> inputs)]

-- instance ToData (Aiken EncoinsInputOnChain) where
--     toBuiltinData (Aiken (iVal, inputs)) = BuiltinData $ List [I iVal, Map (bimap toData I <$> inputs)]

instance ToData (Aiken (TxParams, EncoinsInputOnChain, ProofHash)) where
    toBuiltinData (Aiken ((a1, a2, fees), (iVal, inputs), proofHash)) = do
        let a1' = toData a1
            a2' = toData a2
            fees' = toData fees
            proofHash' = toData proofHash
            par' = List [a1', a2', fees']
            iVal' = I iVal
            inputs' = Map (bimap toData I <$> inputs)
            input' = List [iVal', inputs']
        BuiltinData $ List [par', input', proofHash']

instance ToData (Aiken EncoinsRedeemerOnChain) where
    toBuiltinData (Aiken ((a1, a2, fees), (iVal, inputs), proofHash, sig)) = do
        let a1' = toData a1
            a2' = toData a2
            fees' = toData fees
            proofHash' = toData proofHash
            par' = List [a1', a2', fees']
            iVal' = I iVal
            inputs' = Map (bimap toData I <$> inputs)
            input' = List [iVal', inputs']
            sig' = toData sig
        BuiltinData $ List [par', input', proofHash', sig']

----------------------------------- ENCOINS Minting Policy ---------------------------------------

hashRedeemer :: EncoinsRedeemerOnChain -> BuiltinByteString
hashRedeemer (a, b, c, _) = sha2_256 . serialiseData . toBuiltinData $ Aiken (a, b, c)

encoinsPolicy :: EncoinsProtocolParams -> MintingPolicy
encoinsPolicy = unsafeParameterizedMintingPolicyFromCBOR encoinsPolicyCheck . Aiken . toEncoinsPolicyParams

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
ledgerValidator = unsafeParameterizedValidatorFromCBOR ledgerValidatorCheck . Aiken . encoinsSymbol

ledgerValidatorV :: EncoinsProtocolParams -> Versioned Validator
ledgerValidatorV = flip Versioned PlutusV2 . ledgerValidator

ledgerValidatorHash :: EncoinsProtocolParams -> ValidatorHash
ledgerValidatorHash = validatorHash . ledgerValidator

ledgerValidatorAddress :: EncoinsProtocolParams -> Address
ledgerValidatorAddress par = Address
    (ScriptCredential (ledgerValidatorHash par))
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $
        toBuiltin $ fromJust $ decodeHex "3c2c08be107291be8d71bbb32da11f3b9761b0991f2a6f6940f4f390")