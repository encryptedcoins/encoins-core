{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Script where

import           Cardano.Api                (NetworkId (..), NetworkMagic (..))
import           Cardano.Node.Emulator      (Params (..), pParamsFromProtocolParams)
import           Data.Aeson                 (decode, eitherDecodeFileStrict)
import           Data.ByteString.Lazy       (readFile)
import           Data.Default               (def)
import           Data.Maybe                 (fromJust)
import           ENCOINS.Core.OnChain
import           Plutus.V2.Ledger.Api
import           PlutusTx.AssocMap          (empty)
import qualified PlutusTx.AssocMap          as PAM
import           PlutusTx.Prelude           (emptyByteString, sha2_256, zero)
import           Prelude                    hiding (readFile)

import           PlutusAppsExtra.Test.Utils (testMintingPolicy, testValidator, genPubKeyAddress)

import           Control.Monad              (replicateM, join)
import           Data.Bifunctor             (Bifunctor (..))
import           Data.Function              (on)
import           Data.List                  (sortBy, partition)
import           ENCOINS.BaseTypes          (MintingPolarity (..))
import           ENCOINS.Bulletproofs       (Secret (Secret), bulletproof, fromSecret, parseBulletproofParams, polarityToInteger)
import           ENCOINS.Core.OffChain      (EncoinsMode (..), mkEncoinsRedeemerOnChain, protocolFee, protocolFeeValue)
import           ENCOINS.Crypto.Field       (toFieldElement)
import           Ledger.Ada                 (lovelaceValueOf, adaValueOf)
import           PlutusTx.Extra.ByteString  (ToBuiltinByteString (..))
import           System.Random              (randomIO, randomRIO)
import           Gen                        (BurnRequest, EncoinsRequest (..), MintRequest (..), MixedRequest)
import           Test.Hspec                 (Expectation, describe, hspec, it)
import           Test.QuickCheck            (Testable (property), withMaxSuccess, Arbitrary (arbitrary), generate)
import qualified Ledger.Value as Value

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
                it "Mint" $ property $ withMaxSuccess 10 $ withMPTest @MintRequest  WalletMode
                it "Burn" $ property $ withMaxSuccess 10 $ withMPTest @BurnRequest  WalletMode
                it "Mix"  $ property $ withMaxSuccess 10 $ withMPTest @MixedRequest WalletMode
            -- describe "Ledger mode" $ do
            --     it "Mint" $ property $ withMaxSuccess 10 $ withMPTest @MintRequest  LedgerMode
            --     it "Burn" $ property $ withMaxSuccess 10 $ withMPTest @BurnRequest  LedgerMode
            --     it "Mix"  $ property $ withMaxSuccess 10 $ withMPTest @MixedRequest LedgerMode

ledgerValidatorTest :: Params -> BuiltinByteString -> Expectation
ledgerValidatorTest ledgerParams verifierPKH = do
    encoinsParams <- genEncoinsParams verifierPKH
    let encoinsCS = encoinsSymbol encoinsParams
    testValidator
        ledgerParams
        (ledgerValidator encoinsParams)
        ()
        (42 :: Integer)
        (ScriptContext
            emptyInfo {txInfoRedeemers = PAM.singleton (Minting encoinsCS) (Redeemer (BuiltinData $ Constr 0 []))}
            (Minting encoinsCS))

mintingPolicyTest :: forall req. EncoinsRequest req =>  Params -> BuiltinByteString -> BuiltinByteString -> EncoinsMode-> req -> Expectation
mintingPolicyTest ledgerParams verifierPKH verifierPrvKey mode (extractRequest -> req) = do
    encoinsParams <- genEncoinsParams verifierPKH
    gammas <- replicateM (length req) randomIO
    randomness <- randomIO
    changeAddress <- genPubKeyAddress
    bulletproofSetup <- randomIO
    let ledgerAddress = ledgerValidatorAddress encoinsParams
        encoinsCs = encoinsSymbol encoinsParams

        ps            = map (\i -> if i < 0 then Burn else Mint) req
        secrets       = zipWith (\i g -> Secret g (toFieldElement i)) req gammas
        v             = sum $ map (fst . fromSecret bulletproofSetup) secrets
        par           = (ledgerAddress, changeAddress, 2*protocolFee mode v) :: TxParams
        bp            = parseBulletproofParams $ sha2_256 $ toBytes par
        inputs        = zipWith (\(_, bs) p -> (bs, p)) (map (fromSecret bulletproofSetup) secrets) ps
        (_, _, proof) = bulletproof bulletproofSetup bp secrets ps randomness
        signature  = ""
        red = (par, (v, inputs), proof, signature)
        redOnChain = mkEncoinsRedeemerOnChain verifierPrvKey red

        beaconSymbol = beaconCurrencySymbol encoinsParams
        beaconWithFee = singleton beaconSymbol beaconTokenName 1 <> protocolFeeValue mode v
        txMint =  Value . PAM.fromList . (:[]) . (encoinsCs,) . PAM.fromList . sortBy (compare `on` fst) 
            . fmap (bimap TokenName polarityToInteger) $ inputs

    testMintingPolicy
        ledgerParams
        (encoinsPolicy encoinsParams)
        (Aiken redOnChain)
        (ScriptContext
            emptyInfo
                { txInfoReferenceInputs = [TxInInfo (TxOutRef (TxId "") 0) (TxOut ledgerAddress beaconWithFee NoOutputDatum Nothing)]
                , txInfoMint = txMint
                , txInfoOutputs = [TxOut ledgerAddress (lovelaceValueOf $ v * 1_000_000) (OutputDatum (Datum $ toBuiltinData ())) Nothing]
                }
            (Minting encoinsCs))

genEncoinsParams :: BuiltinByteString -> IO EncoinsProtocolParams
genEncoinsParams verifierPKH = generate $ (,,verifierPKH) <$> arbitrary <*> arbitrary

emptyInfo :: TxInfo
emptyInfo = TxInfo {
    txInfoInputs = [],
    txInfoReferenceInputs = [],
    txInfoOutputs = [],
    txInfoFee = zero,
    txInfoMint = zero,
    txInfoDCert = [],
    txInfoWdrl = empty,
    txInfoValidRange = always,
    txInfoSignatories = [],
    txInfoRedeemers = empty,
    txInfoData = empty,
    txInfoId = TxId emptyByteString
}