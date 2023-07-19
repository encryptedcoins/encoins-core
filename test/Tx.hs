{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Tx where

import           Cardano.Node.Emulator         (Params (..))
import           Control.Lens
import           Control.Lens.Tuple            (_2, _3)
import           Control.Monad.State           (evalStateT, gets, modify, when)
import           Data.Aeson                    (eitherDecodeFileStrict)
import qualified Data.ByteString               as BS
import           Data.Either                   (isRight)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)
import           ENCOINS.Core.OffChain         (EncoinsMode (..), encoinsTx)
import           ENCOINS.Core.OnChain          (beaconAssetClass, encoinsStakeValidatorHash, ledgerValidatorAddress,
                                                minAdaTxOutInLedger, minTxOutValueInLedger, stakeOwnerToken)
import           Gen                           (BurnRequest, EncoinsRequest (..), MintRequest, MixedRequest)
import           Internal                      (TestConfig (..))
import           Ledger                        (Address (..), DecoratedTxOut (..), StakeValidatorHash (..), TxId (..),
                                                TxOutRef (..), ValidatorHash (..), Value, _decoratedTxOutAddress,
                                                decoratedTxOutValue)
import           Ledger.Ada                    (lovelaceValueOf)
import           Ledger.Value                  (assetClassValue, scale)
import qualified Ledger.Value                  as Value
import           Plutus.V2.Ledger.Api          (Credential (..), StakingCredential (..), toBuiltin)
import           Plutus.V2.Ledger.Contexts     (TxInfo (..))
import           PlutusAppsExtra.Test.Utils    (TxTestM, buildTx, getProtocolParams, runTxTest)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           PlutusAppsExtra.Utils.Datum   (inlinedUnitInTxOut)
import           PlutusTx.Builtins             (BuiltinByteString)
import           Script                        (TestEnv (..), genTestEnv)
import           Test.Hspec                    (Expectation, context, describe, hspec, it, parallel, shouldSatisfy)
import           Test.QuickCheck               (property)

txSpec :: IO ()
txSpec = do
    TestConfig{..} <- either error id <$> eitherDecodeFileStrict "test/testConfig.json"
    verifierPKH    <- either error id <$> eitherDecodeFileStrict tcVerifierPkhFile
    verifierPrvKey <- either error id <$> eitherDecodeFileStrict tcVerifierPrvKeyFile
    pParams        <- getProtocolParams tcProtocolParamsFile tcNetworkId
    let withTxTest :: forall req. EncoinsRequest req => EncoinsMode -> req -> Expectation
        withTxTest = encoinsTxTest pParams verifierPKH verifierPrvKey
    hspec $ parallel $ describe "encoinsTx" $ do
        context "fixed utxos" $ do
            context "wallet mode" $ do
                it "mint" $ property $ withTxTest @MintRequest  WalletMode
                it "burn" $ property $ withTxTest @BurnRequest  WalletMode 
                it "mix"  $ property $ withTxTest @MixedRequest WalletMode
            context "ledger mode" $ do
                it "mint" $ property $ withTxTest @MintRequest  LedgerMode
                it "burn" $ property $ withTxTest @BurnRequest  LedgerMode
                it "mix"  $ property $ withTxTest @MixedRequest LedgerMode

encoinsTxTest :: EncoinsRequest req =>
    Params -> BuiltinByteString -> BuiltinByteString -> EncoinsMode -> req -> Expectation
encoinsTxTest pParams verifierPKH verifierPrvKey mode req = do
        TestEnv{..} <- genTestEnv verifierPKH verifierPrvKey mode req
        let addrRelay    = fromJust $ bech32ToAddress "addr_test1qqmg05vsxgf04lke32qkaqt09rt690qzulujazhk39xtkcqnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdqheec82"
            addrTreasury = fromJust $ bech32ToAddress "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"
        runTxTest $ do
            addTxInputs mode TestEnv{..}
            addSetupTokens TestEnv{..}
            addAdaTo maxTxFee teChangeAddr
            buildTx pParams Nothing teChangeAddr [encoinsTx (addrRelay, addrTreasury) teEncoinsParams teRedeemer mode]
    where
        addTxInputs WalletMode TestEnv{..} = do
            when (teV + teFees + teDeposits > 0) $ addAdaTo (teV + teFees + teDeposits) teChangeAddr
            when (teV < 2) $ addLovelaceTo ((max 0 (-teV) * 1_000_000) + minAdaTxOutInLedger) (ledgerValidatorAddress teEncoinsParams)
            addValueTo (fst $ Value.split $ txInfoMint teTxInfo) teChangeAddr
        addTxInputs LedgerMode TestEnv{..} = do
            when (teV + teDeposits < 0) $ addLovelaceTo ((-teV - teDeposits) * 1_000_000 + minAdaTxOutInLedger) (ledgerValidatorAddress teEncoinsParams)
            when (teV + teFees + teDeposits > 0) $ addAdaTo (teV + teFees + teDeposits) teChangeAddr
            addValueTo (fst (Value.split $ txInfoMint teTxInfo) <> scale 2 minTxOutValueInLedger) (ledgerValidatorAddress teEncoinsParams)
        maxTxFee = 4

addValueTo :: Value -> Address -> TxTestM ()
addValueTo v addr = gets (Map.toList . Map.filter ((== addr) . _decoratedTxOutAddress)) >>= \case
    (ref, txOut):_ -> modify $ Map.insert ref (txOut & decoratedTxOutValue %~ (<> v))
    _              -> do
        let out = case addr of
                (Address (PubKeyCredential pkh) mbSc) -> PublicKeyDecoratedTxOut pkh mbSc v Nothing Nothing
                (Address (ScriptCredential vh)  mbSc) -> ScriptDecoratedTxOut vh mbSc v inlinedUnitInTxOut Nothing Nothing
        ref <- genStateTxOutRef
        modify (Map.singleton ref out <>)

addLovelaceTo :: Integer -> Address -> TxTestM ()
addLovelaceTo i = addValueTo (lovelaceValueOf i)

addAdaTo :: Integer -> Address -> TxTestM ()
addAdaTo i = addLovelaceTo (i * 1_000_000)

addSetupTokens :: TestEnv -> TxTestM ()
addSetupTokens TestEnv{..} = do
    -- Set stake owner token
    modify (<> [(teEncoinsParams ^. _1, ScriptDecoratedTxOut
        (let Address (ScriptCredential vh) _ = ledgerValidatorAddress teEncoinsParams in vh)
        (let StakeValidatorHash vh = encoinsStakeValidatorHash teEncoinsParams
         in Just $ StakingHash $ ScriptCredential $ ValidatorHash vh)
        (stakeOwnerToken teEncoinsParams)
        inlinedUnitInTxOut
        Nothing
        Nothing
        )])

    -- Set beacon token
    let Address (ScriptCredential vh) (Just (StakingHash vh')) = ledgerValidatorAddress teEncoinsParams
    modify (<> [(teEncoinsParams ^.  _2, ScriptDecoratedTxOut
        vh
        (Just $ StakingHash vh')
        (assetClassValue (beaconAssetClass teEncoinsParams) 1)
        inlinedUnitInTxOut
        Nothing
        Nothing
        )])

genStateTxOutRef :: TxTestM TxOutRef
genStateTxOutRef = do
    i <- gets length
    pure $ TxOutRef (TxId . toBuiltin $ BS.concat $ replicate 31 (BS.singleton 0) <> [BS.singleton (fromIntegral i)]) 0