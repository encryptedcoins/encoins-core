{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Script where

import           Cardano.Api                (NetworkId (..), NetworkMagic (..))
import           Cardano.Node.Emulator      (Params (..), pParamsFromProtocolParams)
import           Control.Monad              (replicateM)
import           Data.Aeson                 (decode, eitherDecodeFileStrict)
import           Data.Bifunctor             (Bifunctor (..))
import           Data.ByteString.Lazy       (readFile)
import           Data.Default               (def)
import           Data.Function              (on)
import           Data.List                  (sortBy)
import           Data.Maybe                 (fromJust)
import           ENCOINS.BaseTypes          (MintingPolarity (..))
import           ENCOINS.Bulletproofs       (Secret (Secret), bulletproof, fromSecret, parseBulletproofParams, polarityToInteger)
import           ENCOINS.Core.OffChain      (EncoinsMode (..), mkEncoinsRedeemerOnChain, protocolFee)
import           ENCOINS.Core.OnChain
import           ENCOINS.Crypto.Field       (toFieldElement)
import           Gen                        (BurnRequest (..), EncoinsRequest (..), MintRequest (..), MixedRequest, genEncoinsParams)
import           Ledger.Ada                 (lovelaceValueOf)
import           Ledger.Value               (adaOnlyValue, isZero, leq)
import           Plutus.V2.Ledger.Api
import           PlutusAppsExtra.Test.Utils (emptyInfo, genPubKeyAddress, testMintingPolicy, testValidator)
import qualified PlutusTx.AssocMap          as PAM
import           PlutusTx.Extra.ByteString  (ToBuiltinByteString (..))
import           PlutusTx.Prelude           (Group (inv), sha2_256, zero)
import           Prelude                    hiding (readFile)
import           System.Random              (randomIO)
import           Test.Hspec                 (Expectation, describe, hspec, it)
import           Test.QuickCheck            (Testable (property))

runScriptTest :: IO ()
runScriptTest = do
    pp <- fromJust . decode <$> readFile "test/protocol-parameters.json"
    verifierPrvKey <- either error id <$> eitherDecodeFileStrict "test/verifierPrvKey.json"
    verifierPKH    <- either error id <$> eitherDecodeFileStrict "test/verifierPKH.json"
    let networkId = Testnet $ NetworkMagic 1
        ledgerParams = Params def (pParamsFromProtocolParams pp) networkId

    hspec $ describe "Script tests" $ do

        it "Ledger validator" $ ledgerValidatorTest ledgerParams verifierPKH

        describe "Minting policy" $ do
            let withMPTest :: forall req. EncoinsRequest req => EncoinsMode -> req -> Expectation
                withMPTest = mintingPolicyTest ledgerParams verifierPKH verifierPrvKey
            describe "Wallet mode" $ do
                it "Mint" $ property $ withMPTest @MintRequest  WalletMode
                it "Burn" $ property $ withMPTest @BurnRequest  WalletMode
                it "Mix"  $ property $ withMPTest @MixedRequest WalletMode
            describe "Ledger mode" $ do
                it "Mint" $ property $ withMPTest @MintRequest  LedgerMode
                it "Burn" $ property $ withMPTest @BurnRequest  LedgerMode $ BurnRequest [-4]
                it "Burn" $ property $ withMPTest @BurnRequest  LedgerMode
                it "Mix"  $ property $ withMPTest @MixedRequest LedgerMode

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

mintingPolicyTest :: forall req. EncoinsRequest req => Params -> BuiltinByteString -> BuiltinByteString -> EncoinsMode-> req -> Expectation
mintingPolicyTest ledgerParams verifierPKH verifierPrvKey mode req = do
    TestEnv{..} <- genTestEnv verifierPKH verifierPrvKey mode req
    let encoinsCs = encoinsSymbol teEncoinsParams
    testMintingPolicy
        ledgerParams
        (encoinsPolicy teEncoinsParams)
        (Aiken teRedeemer)
        (ScriptContext
            teTxInfo
            (Minting encoinsCs))

data TestEnv = TestEnv
    { teTxInfo        :: TxInfo
    , teReq           :: [Integer]
    , teEncoinsParams :: EncoinsProtocolParams
    , teRedeemer      :: EncoinsRedeemerOnChain
    , teChangeAddr    :: Address
    , teV             :: Integer
    , teFees          :: Integer
    , teDeposits      :: Integer
    }

genTestEnv :: forall req. EncoinsRequest req =>
    BuiltinByteString -> BuiltinByteString -> EncoinsMode-> req -> IO TestEnv
genTestEnv verifierPKH verifierPrvKey mode (extractRequest -> req) = do
    encoinsParams <- genEncoinsParams verifierPKH
    gammas <- replicateM (length req) randomIO
    randomness <- randomIO
    changeAddress <- genPubKeyAddress
    bulletproofSetup <- randomIO
    let ledgerAddress = ledgerValidatorAddress encoinsParams
        encoinsCs = encoinsSymbol encoinsParams

        ps            = map (\i -> if i < 0 then Burn else Mint) req
        secrets       = zipWith (\i g -> Secret g (toFieldElement i)) req gammas
        v             = sum req
        fees          = 2 * protocolFee mode v
        par           = (ledgerAddress, changeAddress, fees) :: TxParams
        bp            = parseBulletproofParams $ sha2_256 $ toBytes par
        inputs        = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
        (_, _, proof) = bulletproof bulletproofSetup bp secrets ps randomness
        signature  = ""
        red = (par, (v, inputs), proof, signature)
        redOnChain = mkEncoinsRedeemerOnChain verifierPrvKey red

        beaconSymbol = beaconCurrencySymbol encoinsParams
        beacon = singleton beaconSymbol beaconTokenName 1
        mint = sortBy (compare `on` fst) $ bimap TokenName polarityToInteger <$> inputs
        txMint = Value . PAM.fromList . (:[]) . (encoinsCs,) . PAM.fromList $ mint
        val = lovelaceValueOf $ v * 1_000_000
        deposits = case mode of
            WalletMode -> 0
            LedgerMode -> depositMultiplier * sum (polarityToInteger <$> ps)
        valDeposits = lovelaceValueOf $ deposits * 1_000_000

        tokenNameToVal name = mkEncoinsValue encoinsCs [(name, 1)]
        ledgerInVal  = map ((minTxOutValueInLedger <>) . tokenNameToVal . fst) $ filter ((== -1) . snd) mint
        ledgerOutVal = map ((minTxOutValueInLedger <>) . tokenNameToVal . fst) $ filter ((==  1) . snd) mint
        (ledgerInVal', ledgerOutVal') = balanceLedgerInsOuts ledgerInVal ledgerOutVal (val <> valDeposits <> txMint)

        ledgerIns
            | mode == WalletMode && v > 0 = [mkLedgerTxIn ledgerAddress minTxOutValueInLedger]
            | mode == WalletMode          = [mkLedgerTxIn ledgerAddress $ inv val <> minTxOutValueInLedger]
            | otherwise                   =  mkLedgerTxIn ledgerAddress <$> ledgerInVal'

        ledgerOuts
            | mode == WalletMode && v > 0 = [mkLedgerTxOut ledgerAddress $ val <> minTxOutValueInLedger]
            | mode == WalletMode          = [mkLedgerTxOut ledgerAddress minTxOutValueInLedger]
            | otherwise                   =  mkLedgerTxOut ledgerAddress <$> ledgerOutVal'

        waletTokensIn = if mode == WalletMode then inv $ mkEncoinsValue encoinsCs $ map (negate <$>) $ filter ((== (-1)) . snd) mint else mempty
        walletIns
            | v + fees + deposits > 0 = [mkWalletTxIn changeAddress $ lovelaceValueOf ((v + fees + deposits) * 1_000_000) <> waletTokensIn]
            | otherwise               = [mkWalletTxIn changeAddress waletTokensIn]

        walletTokensOut = if mode == WalletMode then mkEncoinsValue encoinsCs $ filter ((== 1) . snd) mint else mempty
        walletOuts
            | v + fees + deposits > 0 = [mkWalletTxOut changeAddress walletTokensOut]
            | otherwise               = [mkWalletTxOut changeAddress (walletTokensOut <> lovelaceValueOf (-(v + fees + deposits) * 1_000_000))]
    let txInfo = emptyInfo
            { txInfoReferenceInputs = [mkLedgerTxIn ledgerAddress beacon]
            , txInfoMint = txMint
            , txInfoInputs = ledgerIns <> walletIns
            , txInfoOutputs = ledgerOuts <> walletOuts
            }

    return $ TestEnv
        { teTxInfo        = txInfo
        , teReq           = req
        , teEncoinsParams = encoinsParams
        , teRedeemer      = redOnChain
        , teChangeAddr    = changeAddress
        , teV             = v
        , teFees          = fees
        , teDeposits      = deposits
        }

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