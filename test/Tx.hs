{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module Tx where
import           Cardano.Node.Emulator         (Params (..))
import           Control.Lens                  (Field1 (_1), Field2 (_2), (%~), (&), (^.))
import           Control.Monad                 (forM_, replicateM, replicateM_)
import           Control.Monad.State           (MonadIO (..), evalStateT, gets, modify, when)
import           Data.Aeson                    (eitherDecodeFileStrict)
import qualified Data.ByteString               as BS
import           Data.Default                  (Default (def))
import           Data.Digits                   (digits)
import           Data.Either                   (isRight)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)
import           ENCOINS.Core.OffChain         (EncoinsMode (..), encoinsTx, protocolFeeValue)
import           ENCOINS.Core.OnChain          (beaconAssetClass, encoinsSymbol, ledgerValidatorAddress, minAdaTxOutInLedger,
                                                minMaxTxOutValueInLedger, minTxOutValueInLedger, stakeOwnerToken)
import           Internal                      (TestConfig (..), TestEnv (..), TestSpecification (..), genRequest, genTestEnv,
                                                getSpecifications)
import           Ledger                        (Address (..), DecoratedTxOut (..), TxId (..), TxOutRef (..), Value,
                                                _decoratedTxOutAddress, decoratedTxOutValue, toCardanoValue)
import qualified Plutus.Script.Utils.Ada       as P
import qualified Plutus.Script.Utils.Value     as P
import           Plutus.V2.Ledger.Api          (Credential (..), CurrencySymbol (..), TokenName (..), toBuiltin)
import           PlutusAppsExtra.Test.Utils    (TxTestM, buildTx, getProtocolParams, isOutOfResoursesError)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           PlutusAppsExtra.Utils.Datum   (inlinedUnitInTxOut)
import qualified PlutusTx.AssocMap             as PAM
import           PlutusTx.Builtins             (BuiltinByteString)
import           Test.Hspec                    (Spec, SpecWith, context, describe, hspec, it, runIO, shouldSatisfy)
import           Test.QuickCheck               (Arbitrary (arbitrary), Property, choose, discard, forAll, generate, property,
                                                withMaxSuccess)

txSpec :: Spec
txSpec = do
    TestConfig{..}      <- runIO $ either error id <$> eitherDecodeFileStrict "test/configuration/testConfig.json"
    verifierPKH         <- runIO $ either error id <$> eitherDecodeFileStrict tcVerifierPkhFile
    verifierPrvKey      <- runIO $ either error id <$> eitherDecodeFileStrict tcVerifierPrvKeyFile
    pParams             <- runIO $ getProtocolParams tcProtocolParamsFile tcNetworkId
    testSpecsifications <- runIO getSpecifications
    let testTx = encoinsTxTest pParams verifierPKH verifierPrvKey

    describe "encoinsTx" $ do

        context "no specification" $ do
            it "wallet mode" $ testTx def{tsMode = WalletMode}
            it "ledger mode" $ testTx def{tsMode = LedgerMode}

        context "specifications" $ do
            forM_ testSpecsifications $ \(name, tSpec) -> do
                context (name <> ":") $ it (show tSpec) $ do
                    -- Tests that should fail otherwise discard their result, so there is no need to run more than one test
                    let setLimit = if tsShouldFail tSpec then withMaxSuccess 1 else id
                    setLimit $ testTx tSpec

encoinsTxTest :: Params -> BuiltinByteString -> BuiltinByteString -> TestSpecification -> Property
encoinsTxTest pParams verifierPKH verifierPrvKey TestSpecification{..} = property $
    forAll (genRequest tsMaxAdaInSingleToken tsMode) $ \req -> do
        TestEnv{..} <- genTestEnv verifierPKH verifierPrvKey req
        res <- evalStateT (runTest TestEnv{..}) mempty
        if not tsShouldFail
        -- A test that should't have failed by mem/cpu limit
        then res `shouldSatisfy` isRight
        else case res of
            -- A test that should have failed, and it did
            Left  _ -> res `shouldSatisfy` isOutOfResoursesError
            -- A test that should have failed, but it didn't
            Right _ -> putStrLn "Discarded." >> discard
    where
        runTest TestEnv{..} = do
            setTxInputs TestEnv{..}
            setSetupTokens TestEnv{..}
            addAdaTo teChangeAddr maxTxFee
            let addrRelay    = fromJust $ bech32ToAddress "addr_test1qqmg05vsxgf04lke32qkaqt09rt690qzulujazhk39xtkcqnt9a4spnfrrlpp7puw2lcx2zudf49ewyza4q9ha08qhdqheec82"
                addrTreasury = fromJust $ bech32ToAddress "addr_test1qzdzazh6ndc9mm4am3fafz6udq93tmdyfrm57pqfd3mgctgu4v44ltv85gw703f2dse7tz8geqtm4n9cy6p3lre785cqutvf6a"
            buildTx pParams Nothing teChangeAddr [encoinsTx (addrRelay, addrTreasury) teEncoinsParams teRedeemer tsMode]
        setTxInputs TestEnv{..} = do
            specifyWalletUtxos (teV + teFees + minAdaTxOutInLedger*teDeposits) teChangeAddr
            specifyLedgerUtxos TestEnv{..}
            addValueTo teLedgerAddr minMaxTxOutValueInLedger -- For Condition 7
            let valFee = protocolFeeValue tsMode teV
                encoinsCs = encoinsSymbol teEncoinsParams
                mint = P.Value . PAM.fromList . (:[]) . (encoinsCs,) . PAM.fromList $ teMint
            case tsMode of
                WalletMode -> do
                    when (teV < 2) $ addValueTo teLedgerAddr $ P.lovelaceValueOf (max 0 (-teV) * 1_000_000 + minAdaTxOutInLedger)
                    addValueTo teChangeAddr (fst $ P.split mint)
                LedgerMode -> do
                    when (teV*1_000_000 + minAdaTxOutInLedger*teDeposits < 0) $ addValueTo teLedgerAddr $ P.lovelaceValueOf (-teV*1_000_000 - minAdaTxOutInLedger*teDeposits)
                    addValueTo teLedgerAddr (fst (P.split mint) <> P.scale 2 minTxOutValueInLedger)

        setSetupTokens TestEnv{..} = do
            -- Set stake owner token
            let Address (ScriptCredential vh) sCred = ledgerValidatorAddress teEncoinsParams
                v = either (error . show) id $ toCardanoValue $ stakeOwnerToken teEncoinsParams
            modify $ Map.insert (teEncoinsParams ^. _1) $
                ScriptDecoratedTxOut vh sCred v inlinedUnitInTxOut Nothing Nothing
            -- Set beacon token
            let Address (ScriptCredential vh) sCred = ledgerValidatorAddress teEncoinsParams
                v = either (error . show) id $ toCardanoValue $ P.assetClassValue (beaconAssetClass teEncoinsParams) 1
            modify $ Map.insert (teEncoinsParams ^. _2) $
                ScriptDecoratedTxOut vh sCred v inlinedUnitInTxOut Nothing Nothing

        specifyLedgerUtxos TestEnv{..} = do
            let Address (ScriptCredential vh) sCred = teLedgerAddr
                v = either (error . show) id $ toCardanoValue $ minTxOutValueInLedger <> P.singleton (encoinsSymbol teEncoinsParams) "00000000000000000000000000000000" 1
            replicateM_ tsLedgerUtxosAmt $ genStateTxOutRef >>= modify . flip Map.insert
                (ScriptDecoratedTxOut vh sCred v inlinedUnitInTxOut Nothing Nothing)

        maxTxFee = 4

        specifyWalletUtxos totalV addr = do
            let utxosAmt' = max 1 tsWalletUtxosAmt
                minAdaInUtxo = fromIntegral $ ceiling $ fromIntegral (1_000_000 * totalV) / fromIntegral utxosAmt'
                adaInSingleUtxo = max (P.adaOf $ fromIntegral tsAdaInWalletUtxo) minAdaInUtxo
                Address (PubKeyCredential pkh) sCred = addr
                v = either (error . show) id $ toCardanoValue $ P.toValue adaInSingleUtxo
            -- Create wallet utxos
            replicateM_ utxosAmt' $ genStateTxOutRef >>= modify . flip Map.insert
                (PublicKeyDecoratedTxOut pkh sCred v Nothing Nothing)
            -- Add random tokens
            tokens <- liftIO $ replicateM tsForeignTokensInWalletAmt genRandomToken
            forM_ tokens $ \token -> do
                utxos <- gets $ Map.filter ((== addr) . _decoratedTxOutAddress)
                n <- liftIO $ generate $ choose (0, length utxos - 1)
                let token' = either (error . show) id $ toCardanoValue token
                modify $ Map.adjust (& decoratedTxOutValue %~ (<> token')) (Map.keys utxos !! n)

addValueTo :: Address -> P.Value -> TxTestM ()
addValueTo addr (toCardanoValue -> Right v) = gets (Map.toList . Map.filter ((== addr) . _decoratedTxOutAddress)) >>= \case
    (ref, txOut):_ -> modify $ Map.insert ref (txOut & decoratedTxOutValue %~ (<> v))
    _              -> do
        let out = case addr of
                (Address (PubKeyCredential pkh) mbSc) -> PublicKeyDecoratedTxOut pkh mbSc v Nothing Nothing
                (Address (ScriptCredential vh)  mbSc) -> ScriptDecoratedTxOut vh mbSc v inlinedUnitInTxOut Nothing Nothing
        ref <- genStateTxOutRef
        modify (Map.singleton ref out <>)
addValueTo _ _ = error "addValueTo: isLeft toCardanoValue"

addLovelaceTo :: Address -> Integer -> TxTestM ()
addLovelaceTo addr i = addValueTo addr (P.lovelaceValueOf i)

addAdaTo :: Address -> Integer -> TxTestM ()
addAdaTo addr i = addLovelaceTo addr (i * 1_000_000)

genStateTxOutRef :: TxTestM TxOutRef
genStateTxOutRef = do
    i <- gets ((\xs -> replicate (32 - length xs) 0 <> xs) . digits 256 . (+ 1) . length)
    pure $ TxOutRef (TxId . toBuiltin $ BS.concat $ map (BS.singleton . fromIntegral) i) 0

genRandomToken :: IO P.Value
genRandomToken = do
    cs <- CurrencySymbol . toBuiltin . BS.concat <$> replicateM 28 (BS.singleton <$> generate arbitrary)
    name <- TokenName . toBuiltin . BS.concat <$> replicateM 32 (BS.singleton <$> generate arbitrary)
    pure $ P.singleton cs name 1