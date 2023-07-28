{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module Script where

import           Cardano.Api                (NetworkId (..), NetworkMagic (..))
import           Cardano.Node.Emulator      (Params (..), pParamsFromProtocolParams)
import           Control.Monad              (forM_)
import           Data.Aeson                 (decode, eitherDecodeFileStrict)
import           Data.ByteString.Lazy       (readFile)
import           Data.Default               (def)
import           Data.Maybe                 (fromJust)
import           ENCOINS.Core.OffChain      (EncoinsMode (..))
import           ENCOINS.Core.OnChain
import           Internal                   (TestEnv (..), TestSpecification (..), genEncoinsParams, genRequest, genTestEnv,
                                             getSpecifications)
import           Ledger.Ada                 (lovelaceValueOf)
import           Ledger.Value               (adaOnlyValue, isZero, leq)
import           Plutus.V2.Ledger.Api       (Address, BuiltinByteString, BuiltinData (..), CurrencySymbol, Data (..), Datum (..),
                                             OutputDatum (..), Redeemer (Redeemer), ScriptContext (..), ScriptPurpose (..),
                                             ToData (..), TokenName (..), TxId (..), TxInInfo (..), TxInfo (..), TxOut (..),
                                             TxOutRef (..), Value (..), singleton)
import           PlutusAppsExtra.Test.Utils (emptyInfo, testMintingPolicy, testValidator)
import qualified PlutusTx.AssocMap          as PAM
import           PlutusTx.Prelude           (Group (inv), zero)
import           Prelude                    hiding (readFile)
import           Test.Hspec                 (Expectation, context, describe, hspec, it)
import           Test.QuickCheck            (Property, Testable (property), forAll)

runScriptTest :: IO ()
runScriptTest = do
    pp                  <- fromJust . decode <$> readFile "test/protocol-parameters.json"
    verifierPrvKey      <- either error id <$> eitherDecodeFileStrict "test/verifierPrvKey.json"
    verifierPKH         <- either error id <$> eitherDecodeFileStrict "test/verifierPKH.json"
    testSpecsifications <- getSpecifications
    let networkId = Testnet $ NetworkMagic 1
        ledgerParams = Params def (pParamsFromProtocolParams pp) networkId
        testMp =  mintingPolicyTest ledgerParams verifierPKH verifierPrvKey

    hspec $ describe "script tests" $ do
        
        it "ledger validator" $ ledgerValidatorTest ledgerParams verifierPKH
        
        context "minting policy" $ do
            forM_ testSpecsifications $ \(name, tSpec) -> do
                context (name <> ":") $ context (show tSpec) $ do
                    it "wallet mode" $ testMp WalletMode tSpec
                    it "ledger mode" $ testMp LedgerMode tSpec

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

mintingPolicyTest :: Params -> BuiltinByteString -> BuiltinByteString -> EncoinsMode -> TestSpecification -> Property
mintingPolicyTest ledgerParams verifierPKH verifierPrvKey mode TestSpecification{..} =
    property $ forAll (genRequest tsMaxAdaInSingleToken mode) $ \encoinsRequest -> do
        TestEnv{..} <- genTestEnv verifierPKH verifierPrvKey encoinsRequest
        let encoinsCs = encoinsSymbol teEncoinsParams
            beaconSymbol = beaconCurrencySymbol teEncoinsParams
            beacon = singleton beaconSymbol beaconTokenName 1
            val = lovelaceValueOf $ teV * 1_000_000
            valDeposits = lovelaceValueOf $ teDeposits * 1_000_000

            tokenNameToVal name = mkEncoinsValue encoinsCs [(name, 1)]
            ledgerInVal  = map ((minTxOutValueInLedger <>) . tokenNameToVal . fst) $ filter ((== -1) . snd) teMint
            ledgerOutVal = map ((minTxOutValueInLedger <>) . tokenNameToVal . fst) $ filter ((==  1) . snd) teMint
            txMint = Value . PAM.fromList . (:[]) . (encoinsCs,) . PAM.fromList $ teMint
            (ledgerInVal', ledgerOutVal') = balanceLedgerInsOuts ledgerInVal ledgerOutVal (val <> valDeposits <> txMint)

            ledgerIns
                | mode == WalletMode && teV > 0 = [mkLedgerTxIn teLedgerAddr minTxOutValueInLedger]
                | mode == WalletMode          = [mkLedgerTxIn teLedgerAddr $ inv val <> minTxOutValueInLedger]
                | otherwise                   =  mkLedgerTxIn teLedgerAddr <$> ledgerInVal'

            ledgerOuts
                | mode == WalletMode && teV > 0 = [mkLedgerTxOut teLedgerAddr $ val <> minTxOutValueInLedger]
                | mode == WalletMode            = [mkLedgerTxOut teLedgerAddr minTxOutValueInLedger]
                | otherwise                     =  mkLedgerTxOut teLedgerAddr <$> ledgerOutVal'

            waletTokensIn = if mode == WalletMode then mkEncoinsValue encoinsCs $ map (negate <$>) $ filter ((== (-1)) . snd) teMint else mempty
            walletIns
                | teV + teFees + teDeposits > 0 = [mkWalletTxIn teChangeAddr $ lovelaceValueOf ((teV + teFees + teDeposits) * 1_000_000) <> waletTokensIn]
                | otherwise               = [mkWalletTxIn teChangeAddr waletTokensIn]

            walletTokensOut = if mode == WalletMode then mkEncoinsValue encoinsCs $ filter ((== 1) . snd) teMint else mempty
            walletOuts
                | teV + teFees + teDeposits > 0 = [mkWalletTxOut teChangeAddr walletTokensOut]
                | otherwise               = [mkWalletTxOut teChangeAddr (walletTokensOut <> lovelaceValueOf (-(teV + teFees + teDeposits) * 1_000_000))]
            txInfo = emptyInfo
                { txInfoReferenceInputs = [mkLedgerTxIn teLedgerAddr beacon]
                , txInfoMint            = txMint
                , txInfoInputs          = mconcat
                    [ledgerIns, walletIns, walletSpecifiedInputs  teChangeAddr, ledgerSpecifiedInputs teLedgerAddr encoinsCs]
                , txInfoOutputs         = mconcat
                    [ledgerOuts, walletOuts, walletSpecifiedOutputs teChangeAddr, ledgerSpecifiedOutputs teLedgerAddr encoinsCs]
                }
        
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