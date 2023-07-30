{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module Script where

import           Cardano.Api                (NetworkId (..), NetworkMagic (..))
import           Cardano.Node.Emulator      (Params (..), pParamsFromProtocolParams)
import           Control.Monad              (forM_, when)
import           Data.Aeson                 (decode, eitherDecodeFileStrict)
import           Data.ByteString.Lazy       (readFile)
import           Data.Default               (def)
import           Data.Either                (isLeft)
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Data.Maybe                 (fromJust)
import           ENCOINS.Core.OffChain      (EncoinsMode (..))
import           ENCOINS.Core.OnChain
import           GHC.IO                     (unsafePerformIO)
import           Internal                   (TestConfig (..), TestEnv (..), TestSpecification (..), genEncoinsParams, genRequest, genTestEnv,
                                             getSpecifications)
import           Ledger.Ada                 (lovelaceValueOf, adaValueOf)
import           Ledger.Value               (adaOnlyValue, isZero, leq)
import           Plutus.V2.Ledger.Api       (Address, BuiltinByteString, BuiltinData (..), CurrencySymbol, Data (..), Datum (..),
                                             OutputDatum (..), Redeemer (Redeemer), ScriptContext (..), ScriptPurpose (..),
                                             ToData (..), TokenName (..), TxId (..), TxInInfo (..), TxInfo (..), TxOut (..),
                                             TxOutRef (..), Value (..), singleton)
import           PlutusAppsExtra.Test.Utils (emptyInfo, testMintingPolicy, testValidator, getProtocolParams)
import qualified PlutusTx.AssocMap          as PAM
import           PlutusTx.Prelude           (Group (inv), zero)
import           Prelude                    hiding (readFile)
import           Test.Hspec                 (Expectation, context, describe, expectationFailure, hspec, it, Spec, runIO)
import           Test.QuickCheck            (Property, Testable (property), forAll, ioProperty, whenFail)

scriptSpec :: Spec
scriptSpec = do
    TestConfig{..}      <- runIO $ either error id <$> eitherDecodeFileStrict "test/configuration/testConfig.json"
    verifierPKH         <- runIO $ either error id <$> eitherDecodeFileStrict tcVerifierPkhFile
    verifierPrvKey      <- runIO $ either error id <$> eitherDecodeFileStrict tcVerifierPrvKeyFile
    pParams             <- runIO $ getProtocolParams tcProtocolParamsFile tcNetworkId
    testSpecsifications <- runIO getSpecifications

    let testMp = mintingPolicyTest pParams verifierPKH verifierPrvKey

    describe "script tests" $ do

        it "ledger validator" $ ledgerValidatorTest pParams verifierPKH

        context "minting policy" $ do
                it "wallet mode" $ testMp def{tsMode = WalletMode}
                it "ledger mode" $ testMp def{tsMode = LedgerMode}

ledgerValidatorTest :: Params -> BuiltinByteString -> Expectation
ledgerValidatorTest ledgerParams verifierPKH = do
    encoinsParams <- genEncoinsParams verifierPKH
    let encoinsCS = encoinsSymbol encoinsParams
    testValidator
        ledgerParams
        (ledgerValidator encoinsParams)
        ()
        ()
        (ScriptContext
            emptyInfo {txInfoRedeemers = PAM.singleton (Minting encoinsCS) (Redeemer (BuiltinData $ Constr 0 []))}
            (Minting encoinsCS))

mintingPolicyTest :: Params -> BuiltinByteString -> BuiltinByteString -> TestSpecification -> Property
mintingPolicyTest ledgerParams verifierPKH verifierPrvKey TestSpecification{..} = do
    let txInfoRef = unsafePerformIO $ newIORef (undefined :: TxInfo)
    whenFail (readIORef txInfoRef >>= print) $ property $ forAll (genRequest tsMaxAdaInSingleToken tsMode) $ \encoinsRequest -> do
        TestEnv{..} <- genTestEnv verifierPKH verifierPrvKey encoinsRequest
        let encoinsCs = encoinsSymbol teEncoinsParams
            beaconSymbol = beaconCurrencySymbol teEncoinsParams
            beacon = singleton beaconSymbol beaconTokenName 1
            val = lovelaceValueOf $ teV * 1_000_000
            valDeposits = lovelaceValueOf $ teDeposits * 1_000_000

            tokenNameToVal name = mkEncoinsValue encoinsCs [(name, 1)]
            ledgerInVal  = map ((minTxOutValueInLedger <>) . tokenNameToVal . fst) (filter ((== -1) . snd) teMint)
            ledgerOutVal = minMaxTxOutValueInLedger : map ((minTxOutValueInLedger <>) . tokenNameToVal . fst) (filter ((==  1) . snd) teMint)
            txMint = Value . PAM.fromList . (:[]) . (encoinsCs,) . PAM.fromList $ teMint
            (ledgerInVal', ledgerOutVal') = balanceLedgerInsOuts ledgerInVal ledgerOutVal (val <> valDeposits <> txMint)

            ledgerIns
                | tsMode == WalletMode && teV > 0 = [mkLedgerTxIn teLedgerAddr minMaxTxOutValueInLedger]
                | tsMode == WalletMode            = [mkLedgerTxIn teLedgerAddr $ inv val <> minMaxTxOutValueInLedger]
                | otherwise                       =  mkLedgerTxIn teLedgerAddr <$> ledgerInVal'

            ledgerOuts
                | tsMode == WalletMode && teV > 0 = [mkLedgerTxOut teLedgerAddr $ val <> minMaxTxOutValueInLedger]
                | tsMode == WalletMode            = [mkLedgerTxOut teLedgerAddr minMaxTxOutValueInLedger]
                | otherwise                       =  mkLedgerTxOut teLedgerAddr <$> ledgerOutVal'

            waletTokensIn = if tsMode == WalletMode then mkEncoinsValue encoinsCs $ map (negate <$>) $ filter ((== (-1)) . snd) teMint else mempty
            walletIns
                | teV + teFees + teDeposits > 0 = [mkWalletTxIn teChangeAddr $ lovelaceValueOf ((teV + teFees + teDeposits) * 1_000_000) <> waletTokensIn]
                | otherwise                     = [mkWalletTxIn teChangeAddr waletTokensIn]

            walletTokensOut = if tsMode == WalletMode then mkEncoinsValue encoinsCs $ filter ((== 1) . snd) teMint else mempty
            walletOuts
                | teV + teFees + teDeposits > 0 = [mkWalletTxOut teChangeAddr walletTokensOut]
                | otherwise                     = [mkWalletTxOut teChangeAddr (walletTokensOut <> lovelaceValueOf (-(teV + teFees + teDeposits) * 1_000_000))]
            txInfo = emptyInfo
                { txInfoReferenceInputs = [mkLedgerTxIn teLedgerAddr beacon]
                , txInfoMint            = txMint
                , txInfoInputs          = mconcat
                    [ledgerIns, walletIns, walletSpecifiedInputs  teChangeAddr, ledgerSpecifiedInputs teLedgerAddr encoinsCs]
                , txInfoOutputs         = mconcat
                    [ledgerOuts, walletOuts, walletSpecifiedOutputs teChangeAddr, ledgerSpecifiedOutputs teLedgerAddr encoinsCs]
                }
        writeIORef txInfoRef txInfo
        testMintingPolicy
            ledgerParams
            (encoinsPolicy teEncoinsParams)
            (Aiken teRedeemer)
            (ScriptContext
                txInfo
                (Minting encoinsCs))
    where
        walletSpecifiedOutputs addr = replicate tsWalletUtxosAmt $ mkWalletTxOut addr (lovelaceValueOf $ tsAdaInWalletUtxo * 1_000_000)
        walletSpecifiedInputs addr = map (TxInInfo ref) $ walletSpecifiedOutputs addr
        ledgerSpecifiedOutputs addr cs =
            let v = minTxOutValueInLedger <> singleton cs "0000000000000000000000000000000000000000000000000000000000000000" 1
            in replicate tsLedgerUtxosAmt $ mkLedgerTxOut addr v
        ledgerSpecifiedInputs addr cs = map (TxInInfo ref) $ ledgerSpecifiedOutputs addr cs

mkLedgerTxOut :: Address -> Value -> TxOut
mkLedgerTxOut addr v = TxOut addr v (OutputDatum (Datum $ toBuiltinData ())) Nothing

mkLedgerTxIn :: Address -> Value -> TxInInfo
mkLedgerTxIn addr v = TxInInfo ref $ mkLedgerTxOut addr v

mkWalletTxOut :: Address -> Value -> TxOut
mkWalletTxOut addr v = TxOut addr v NoOutputDatum Nothing

mkWalletTxIn :: Address -> Value -> TxInInfo
mkWalletTxIn addr v = TxInInfo ref $ mkWalletTxOut addr v

mkEncoinsValue :: CurrencySymbol -> [(TokenName, Integer)] -> Value
mkEncoinsValue encoinsCs = Value . PAM.fromList . (:[]) . (encoinsCs,) . PAM.fromList

balanceLedgerInsOuts :: [Value] -> [Value] -> Value -> ([Value], [Value])
balanceLedgerInsOuts ins outs vWithDeposits
    | isZero delta = (ins, outs)
    | delta `leq` zero = case (ins, outs) of
        (is, o:os) -> (is, o <> inv (adaOnlyValue delta)  : os)
        (i:is, []) -> (minTxOutValueInLedger <> i : is, [minTxOutValueInLedger <> inv (adaOnlyValue delta)])
        ([], []) -> ([], [])
    | otherwise = case (ins, outs) of
        (i:is, os) -> (i <> adaOnlyValue delta : is, os)
        ([], o:os) -> ([minTxOutValueInLedger <> adaOnlyValue delta], minTxOutValueInLedger <> o : os)
        ([], []) -> ([], [])
    where
        delta = mconcat outs <> inv (mconcat ins <> vWithDeposits)

ref :: TxOutRef
ref = TxOutRef (TxId "") 0