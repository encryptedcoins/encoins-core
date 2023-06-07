{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
-- {-# OPTIONS_GHC -Wall #-}

module Tx where

import           Cardano.Api                              (NetworkId (..))
import           Cardano.Node.Emulator                    (Params (..))
import           Control.Monad                            (replicateM)
import           Control.Monad.State                      (MonadIO (..), modify)
import           Data.Aeson                               (FromJSON (..), eitherDecodeFileStrict, genericParseJSON)
import           Data.Aeson.Casing                        (aesonPrefix, snakeCase)
import           Data.Bifunctor                           (Bifunctor(..))
import           Data.Maybe                               (fromJust)
import           ENCOINS.Core.OffChain                    (EncoinsMode (..), protocolFee, mkEncoinsRedeemerOnChain, encoinsTx)
import           ENCOINS.Core.OnChain                     (EncoinsProtocolParams, TxParams, encoinsSymbol, ledgerValidatorAddress,
                                                           encoinsPolicy, ledgerValidatorHash, ledgerValidator, encoinsStakeValidator,
                                                           encoinsStakeValidatorHash, stakeOwnerToken, beaconAssetClass)
import           ENCOINS.BaseTypes                        (MintingPolarity(..))
import           ENCOINS.Crypto.Field                     (toFieldElement)
import           ENCOINS.Bulletproofs                     (Secret(..), fromSecret, parseBulletproofParams, bulletproof)
import           GHC.Generics                             (Generic)
import           Gen                                      (BurnRequest, EncoinsRequest (..), MintRequest, MixedRequest)
import           Ledger                                   (Address (..), CurrencySymbol, DecoratedTxOut (..), Language (..),
                                                           StakeValidatorHash (..), TxOutRef (..), Validator (..),
                                                           ValidatorHash (..), Value, Versioned (..), unMintingPolicyScript,
                                                           unStakeValidatorScript, unValidatorScript, validatorHash)
import           Ledger.Ada                               (adaValueOf)
import           Ledger.Value                             (assetClassValue, Value (..), TokenName (..))
import           Plutus.V2.Ledger.Api                     (Credential (..), StakingCredential (..))
import           PlutusAppsExtra.Scripts.CommonValidators (alwaysFalseValidator, alwaysFalseValidatorHash, alwaysFalseValidatorV)
import           PlutusAppsExtra.Test.Utils               (TxTestM, buildTx, genTxOutRef, getProtocolParams, runTxTest,
                                                           withAdaUtxo, withValueUtxo)
import           PlutusAppsExtra.Utils.Address            (bech32ToAddress)
import           PlutusAppsExtra.Utils.Datum              (inlinedUnitInTxOut)
import qualified PlutusTx.AssocMap                        as PAM
import           PlutusTx.Builtins                        (BuiltinByteString, sha2_256)
import           PlutusTx.Extra.ByteString                (toBytes)
import           System.Random                            (randomIO)
import           Test.Hspec                               (Expectation, context, describe, hspec, it, parallel)
import           Test.QuickCheck                          (property, withMaxSuccess)

type HasTxTestEnv =
    ( ?pParams            :: Params
    , ?mode               :: EncoinsMode
    , ?ledgerAddr         :: Address
    , ?verifierPrvKey     :: BuiltinByteString
    , ?encoinsCS          :: CurrencySymbol
    , ?encoinsParams      :: EncoinsProtocolParams
    )

data TestConfig = TestConfig
    { tcProtocolParamsFile :: FilePath
    , tcVerifierPkhFile    :: FilePath
    , tcVerifierPrvKeyFile :: FilePath
    , tcNetworkId          :: NetworkId
    } deriving (Show, Generic)

instance FromJSON TestConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

txSpec :: IO ()
txSpec = do
    TestConfig{..} <- either error id <$> eitherDecodeFileStrict "test/testConfig.json"
    verifierPKH    <- either error id <$> eitherDecodeFileStrict tcVerifierPkhFile
    verifierPrvKey <- either error id <$> eitherDecodeFileStrict tcVerifierPrvKeyFile
    p              <- getProtocolParams tcProtocolParamsFile tcNetworkId

    let refBeacon     = TxOutRef "e4d0238694cbd5138ef39c36e89ae6262a0326f02a621b6cf0942ebc330db011" 0
        refStakeOwner = TxOutRef "d58632be7770afd971564ea6793b8451f3ecfacb7597f6f2f125de01c9e5ae66" 0
        encoinsParams = (refStakeOwner, refBeacon, verifierPKH)
        ledgerAddr    = ledgerValidatorAddress encoinsParams

    let ?pParams         = p
        ?ledgerAddr      = ledgerAddr
        ?verifierPrvKey  = verifierPrvKey
        ?encoinsParams   = encoinsParams
        ?encoinsCS       = encoinsSymbol encoinsParams

    hspec $ parallel $ describe "encoinsTx" $

        context "fixed utxos" $ do

            context "wallet mode" $ do
                let ?mode = WalletMode

                it "mint" $ withMaxSuccess 10 $ property $ encoinsTxTest @MintRequest $ do
                    withSetup encoinsParams
                    withAdaUtxo (?reqSum + 2) ?changeAddr

                it "burn" $ withMaxSuccess 10 $ property $ encoinsTxTest @BurnRequest $ do
                    withSetup encoinsParams
                    withValueUtxo ?burnVal ?changeAddr
                    withAdaUtxo (negate ?reqSum + 2) ?ledgerAddr

                it "mix" $ withMaxSuccess 10 $ property $ encoinsTxTest @MixedRequest $ do
                    withSetup encoinsParams
                    withValueUtxo ?burnVal ?changeAddr
                    withAdaUtxo (abs ?reqSum + 2) $ if ?reqSum > 0 then ?changeAddr else ?ledgerAddr
                    withAdaUtxo 2 ?ledgerAddr
                    withAdaUtxo 2 ?changeAddr

            context "ledger mode" $ do
                let ?mode = LedgerMode

                it "mint" $ withMaxSuccess 10 $ property $ encoinsTxTest @MintRequest $ do
                    withSetup encoinsParams
                    withAdaUtxo (?reqSum + 2) ?changeAddr

                it "burn" $ withMaxSuccess 10 $ property $ encoinsTxTest @BurnRequest $ do
                    withSetup encoinsParams
                    withAdaUtxo (negate ?reqSum + 2) ?ledgerAddr
                    withValueUtxo ?burnVal ?ledgerAddr

                it "mix" $  withMaxSuccess 10 $ property $ encoinsTxTest @MixedRequest $ do
                    withSetup encoinsParams
                    withAdaUtxo (abs ?reqSum + 2) $ if ?reqSum > 0 then ?changeAddr else ?ledgerAddr
                    withValueUtxo ?burnVal ?ledgerAddr
                    withAdaUtxo (abs ?reqSum + 2) ?changeAddr
                    withAdaUtxo (abs ?reqSum + 2) ?ledgerAddr

type HasEncoinsTxTestEnv =
    ( ?changeAddr :: Address
    , ?req        :: [Integer]
    , ?reqSum     :: Integer
    , ?burnVal    :: Value
    , ?collateral :: Maybe TxOutRef
    )

encoinsTxTest :: HasTxTestEnv => EncoinsRequest req => (HasEncoinsTxTestEnv => TxTestM ()) -> req -> Expectation
encoinsTxTest addUtxos (extractRequest -> req) = do
    let changeAddr = fromJust $ bech32ToAddress "addr_test1qqmg05vsxgf04lke32qkaqt09rt690qzulujazhk39xtkcqnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdqheec82"
        collateral = Nothing
        addrRelay = fromJust $ bech32ToAddress "addr_test1qqmg05vsxgf04lke32qkaqt09rt690qzulujazhk39xtkcqnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdqheec82"
        addrTreasury = fromJust $ bech32ToAddress "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"

    gammas <- replicateM (length req) randomIO
    randomness <- randomIO
    bulletproofSetup <- randomIO
    let ps            = map (\i -> if i < 0 then Burn else Mint) req
        secrets       = zipWith (\i g -> Secret g (toFieldElement i)) req gammas
        v             = sum $ map (fst . fromSecret bulletproofSetup) secrets
        par           = (?ledgerAddr, changeAddr, 2*protocolFee ?mode v) :: TxParams
        bp            = parseBulletproofParams $ sha2_256 $ toBytes par
        inputs        = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
        (_, _, proof) = bulletproof bulletproofSetup bp secrets ps randomness
        signature  = ""
        red = (par, (v, inputs), proof, signature)
        redOnChain = mkEncoinsRedeemerOnChain ?verifierPrvKey red

    let ?changeAddr = changeAddr
        ?collateral = Nothing
        ?req        = req
        ?reqSum     = sum req
        ?burnVal    = Value $ PAM.singleton ?encoinsCS $ PAM.fromList $ bimap TokenName (const 1) <$> filter ((== Burn) . snd) inputs

    runTxTest $ do
        addUtxos
        buildTx ?pParams collateral changeAddr [encoinsTx (addrRelay, addrTreasury) ?encoinsParams redOnChain ?mode]

withSetup :: EncoinsProtocolParams -> TxTestM ()
withSetup encoinsParams = do
    ref <- liftIO genTxOutRef
    modify (<> [(ref, ScriptDecoratedTxOut
        (alwaysFalseValidatorHash 20)
        Nothing
        (adaValueOf 2)
        inlinedUnitInTxOut
        (Just $ Versioned (getValidator $ alwaysFalseValidator 20) PlutusV2)
        (Just $ alwaysFalseValidatorV 20)
        )])

    -- Post encoins policy
    ref <- liftIO genTxOutRef
    modify (<> [(ref, ScriptDecoratedTxOut
        (validatorHash $ flip Versioned PlutusV2 $ Validator $ unMintingPolicyScript $ encoinsPolicy encoinsParams)
        Nothing
        (adaValueOf 2)
        inlinedUnitInTxOut
        (Just $ flip Versioned PlutusV2 $ unMintingPolicyScript $ encoinsPolicy encoinsParams)
        (Just $ alwaysFalseValidatorV 20)
        )])

    -- Post ledger validator
    ref <- liftIO genTxOutRef
    modify (<> [(ref, ScriptDecoratedTxOut
        (ledgerValidatorHash encoinsParams)
        (let vh = ledgerValidatorHash encoinsParams in Just $ StakingHash $ ScriptCredential vh)
        (adaValueOf 2)
        inlinedUnitInTxOut
        (Just $ flip Versioned PlutusV2 $ unValidatorScript $ ledgerValidator encoinsParams)
        (Just $ flip Versioned PlutusV2 $ ledgerValidator encoinsParams)
        )])

    -- Post stake validator
    ref <- liftIO genTxOutRef
    modify (<> [(ref, ScriptDecoratedTxOut
        (validatorHash $ flip Versioned PlutusV2 $ Validator $ unStakeValidatorScript $ encoinsStakeValidator encoinsParams)
        Nothing
        (adaValueOf 2)
        inlinedUnitInTxOut
        (Just $ flip Versioned PlutusV2 $ unStakeValidatorScript $ encoinsStakeValidator encoinsParams)
        (Just $ alwaysFalseValidatorV 20)
        )])

    -- Set stake owner token
    modify (<> [((\(a, _, _) -> a) encoinsParams, ScriptDecoratedTxOut
        (let Address (ScriptCredential vh) _ = ledgerValidatorAddress encoinsParams in vh)
        (let StakeValidatorHash vh = encoinsStakeValidatorHash encoinsParams
         in Just $ StakingHash $ ScriptCredential $ ValidatorHash vh)
        (stakeOwnerToken encoinsParams)
        inlinedUnitInTxOut
        Nothing
        Nothing
        )])

    -- Set beacon token
    modify (<> [((\(_, b, _) -> b) encoinsParams, ScriptDecoratedTxOut
        (let Address (ScriptCredential vh) _ = ledgerValidatorAddress encoinsParams in vh)
        (let StakeValidatorHash vh = encoinsStakeValidatorHash encoinsParams
         in Just $ StakingHash $ ScriptCredential $ ValidatorHash vh)
        (assetClassValue (beaconAssetClass encoinsParams) 1 <> adaValueOf 2)
        inlinedUnitInTxOut
        Nothing
        Nothing
        )])