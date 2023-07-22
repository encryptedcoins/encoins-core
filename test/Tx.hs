{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleContexts #-}

module Tx where
import           Cardano.Node.Emulator         (Params (..))
import           Control.Lens                  (Field1 (_1), Field2 (_2), (%~), (&), (^.))
import           Control.Monad                 (forM_, replicateM_)
import           Control.Monad.State           (gets, modify, when)
import           Data.Aeson                    (eitherDecodeFileStrict)
import qualified Data.ByteString               as BS
import           Data.Digits                   (digits)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)
import           ENCOINS.Core.OffChain         (EncoinsMode (..), encoinsTx)
import           ENCOINS.Core.OnChain          (beaconAssetClass, encoinsSymbol, ledgerValidatorAddress, minAdaTxOutInLedger,
                                                minTxOutValueInLedger, stakeOwnerToken)
import           Internal                      (TestConfig (..), TestSpecification (..), genRequest, getSpecifications)
import           Ledger                        (Address (..), DecoratedTxOut (..), TxId (..), TxOutRef (..), Value,
                                                _decoratedTxOutAddress, decoratedTxOutValue)
import           Ledger.Ada                    (lovelaceValueOf)
import           Ledger.Value                  (assetClassValue, scale)
import qualified Ledger.Value                  as Value
import           Plutus.V2.Ledger.Api          (Credential (..), toBuiltin)
import           Plutus.V2.Ledger.Contexts     (TxInfo (..))
import           PlutusAppsExtra.Test.Utils    (TxTestM, buildTx, getProtocolParams, runTxTest)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           PlutusAppsExtra.Utils.Datum   (inlinedUnitInTxOut)
import           PlutusTx.Builtins             (BuiltinByteString)
import           Script                        (TestEnv (..), genTestEnv)
import           Test.Hspec                    (context, describe, hspec, it, parallel)
import           Test.QuickCheck               (Property, forAll, property)

txSpec :: IO ()
txSpec = do
    TestConfig{..}      <- either error id <$> eitherDecodeFileStrict "test/testConfig.json"
    verifierPKH         <- either error id <$> eitherDecodeFileStrict tcVerifierPkhFile
    verifierPrvKey      <- either error id <$> eitherDecodeFileStrict tcVerifierPrvKeyFile
    pParams             <- getProtocolParams tcProtocolParamsFile tcNetworkId
    testSpecsifications <- getSpecifications
    let testTx =  encoinsTxTest pParams verifierPKH verifierPrvKey

    hspec $ parallel $ describe "encoinsTx" $ do
        forM_ testSpecsifications $ \(name, tSpec) -> do
            context (name <> ":") $ context (show tSpec) $ do
                it "wallet mode" $ testTx WalletMode tSpec
                it "ledger mode" $ testTx LedgerMode tSpec

encoinsTxTest :: Params -> BuiltinByteString -> BuiltinByteString -> EncoinsMode -> TestSpecification -> Property
encoinsTxTest pParams verifierPKH verifierPrvKey mode TestSpecification{..} = property $
    forAll (genRequest tsMaxAdaInSingleToken mode) $ \req -> do
        TestEnv{..} <- genTestEnv verifierPKH verifierPrvKey req
        let addrRelay    = fromJust $ bech32ToAddress "addr_test1qqmg05vsxgf04lke32qkaqt09rt690qzulujazhk39xtkcqnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdqheec82"
            addrTreasury = fromJust $ bech32ToAddress "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"
        runTxTest $ do
            setTxInputs TestEnv{..}
            setSetupTokens TestEnv{..}
            addAdaTo teChangeAddr maxTxFee
            specifyUtxos TestEnv{..}
            buildTx pParams Nothing teChangeAddr [encoinsTx (addrRelay, addrTreasury) teEncoinsParams teRedeemer mode]
    where
        setTxInputs TestEnv{..} = case mode of
            WalletMode -> do
                when (teV + teFees + teDeposits > 0) $ addAdaTo teChangeAddr (teV + teFees + teDeposits)
                when (teV < 2) $ addLovelaceTo teLedgerAddr ((max 0 (-teV) * 1_000_000) + minAdaTxOutInLedger)
                addValueTo teChangeAddr (fst $ Value.split $ txInfoMint teTxInfo)
            LedgerMode -> do
                when (teV + teFees + teDeposits > 0) $ addAdaTo teChangeAddr (teV + teFees + teDeposits)
                when (teV + teDeposits < 0) $ addLovelaceTo teLedgerAddr ((-teV - teDeposits) * 1_000_000 + minAdaTxOutInLedger)
                addValueTo teLedgerAddr (fst (Value.split $ txInfoMint teTxInfo) <> scale 2 minTxOutValueInLedger)

        setSetupTokens TestEnv{..} = do
            -- Set stake owner token
            let Address (ScriptCredential vh) sCred = ledgerValidatorAddress teEncoinsParams
            modify $ Map.insert (teEncoinsParams ^. _1) $ 
                ScriptDecoratedTxOut vh sCred (stakeOwnerToken teEncoinsParams) inlinedUnitInTxOut Nothing Nothing
            -- Set beacon token
            let Address (ScriptCredential vh) sCred = ledgerValidatorAddress teEncoinsParams
            modify $ Map.insert (teEncoinsParams ^. _2) $ 
                ScriptDecoratedTxOut vh sCred (assetClassValue (beaconAssetClass teEncoinsParams) 1) inlinedUnitInTxOut Nothing Nothing
        
        specifyUtxos TestEnv{..} = do
            let Address (PubKeyCredential pkh) sCred = teChangeAddr
                v = lovelaceValueOf $ tsAdaInWalletUtxo * 1_000_000
            replicateM_ tsWalletUtxosAmt $ genStateTxOutRef >>= modify . flip Map.insert
                (PublicKeyDecoratedTxOut pkh sCred v Nothing Nothing)
            let Address (ScriptCredential vh) sCred = teLedgerAddr
                v = minTxOutValueInLedger <> Value.singleton (encoinsSymbol teEncoinsParams) "" 1
            replicateM_ tsLedgerUtxosAmt $ genStateTxOutRef >>= modify . flip Map.insert
                (ScriptDecoratedTxOut vh sCred v inlinedUnitInTxOut Nothing Nothing)
        
        maxTxFee = 4

addValueTo :: Address -> Value -> TxTestM ()
addValueTo addr v = gets (Map.toList . Map.filter ((== addr) . _decoratedTxOutAddress)) >>= \case
    (ref, txOut):_ -> modify $ Map.insert ref (txOut & decoratedTxOutValue %~ (<> v))
    _              -> do
        let out = case addr of
                (Address (PubKeyCredential pkh) mbSc) -> PublicKeyDecoratedTxOut pkh mbSc v Nothing Nothing
                (Address (ScriptCredential vh)  mbSc) -> ScriptDecoratedTxOut vh mbSc v inlinedUnitInTxOut Nothing Nothing
        ref <- genStateTxOutRef
        modify (Map.singleton ref out <>)

addLovelaceTo :: Address -> Integer -> TxTestM ()
addLovelaceTo addr i = addValueTo addr (lovelaceValueOf i)

addAdaTo :: Address -> Integer -> TxTestM ()
addAdaTo addr i = addLovelaceTo addr (i * 1_000_000)

genStateTxOutRef :: TxTestM TxOutRef
genStateTxOutRef = do
    i <- gets ((\xs -> replicate (32 - length xs) 0 <> xs) . digits 256 . (+ 1) . length)
    pure $ TxOutRef (TxId . toBuiltin $ BS.concat $ map (BS.singleton . fromIntegral) i) 0