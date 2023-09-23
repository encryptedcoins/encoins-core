{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Internal where

import           Cardano.Api                (NetworkId)
import           Control.Monad              (forM, replicateM)
import           Data.Aeson                 (FromJSON (..), decodeFileStrict, genericParseJSON)
import           Data.Aeson.Casing          (aesonPrefix, snakeCase)
import           Data.Bifunctor             (Bifunctor (..))
import           Data.Default               (Default (..))
import           Data.Function              (on)
import           Data.Functor               ((<&>))
import           Data.List                  (sortBy)
import           Data.Map                   (fromList, toList)
import           Data.Maybe                 (catMaybes)
import           ENCOINS.BaseTypes          (MintingPolarity (..), fromGroupElement)
import           ENCOINS.Bulletproofs       (Input (..), Secret (Secret), bulletproof, fromSecret, parseBulletproofParams,
                                             polarityToInteger)
import           ENCOINS.Core.OffChain      (EncoinsMode (..), mkEncoinsRedeemerOnChain, protocolFee)
import           ENCOINS.Core.OnChain
import           ENCOINS.Crypto.Field       (toFieldElement)
import           GHC.Generics               (Generic)
import           Plutus.V2.Ledger.Api       (Address, BuiltinByteString, TokenName (..))
import           PlutusAppsExtra.Test.Utils (genPubKeyAddress, genTxOutRef)
import           PlutusTx.Extra.ByteString  (ToBuiltinByteString (..))
import           PlutusTx.Prelude           (sha2_256)
import           Prelude                    hiding (readFile)
import           System.Directory           (listDirectory)
import           System.Random              (randomIO)
import           Test.QuickCheck            (Arbitrary (..), Gen, choose, shuffle, suchThat)

data TestConfig = TestConfig
    { tcProtocolParamsFile :: FilePath
    , tcVerifierPkhFile    :: FilePath
    , tcVerifierPrvKeyFile :: FilePath
    , tcNetworkId          :: NetworkId
    } deriving (Show, Generic)

instance FromJSON TestConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data TestSpecification = TestSpecification
    { tsWalletUtxosAmt           :: Int
    , tsAdaInWalletUtxo          :: Integer
    , tsForeignTokensInWalletAmt :: Int
    , tsLedgerUtxosAmt           :: Int
    , tsMaxAdaInSingleToken      :: Integer
    , tsShouldFail               :: Bool
    , tsMode                     :: EncoinsMode
    } deriving (Show, Generic)

instance Default TestSpecification where
    def = TestSpecification 0 0 0 0 1000 False WalletMode

instance FromJSON TestSpecification where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

getSpecifications :: IO [(String, TestSpecification)]
getSpecifications = do
    let d = "test/specifications"
    names <- listDirectory d
    fmap catMaybes . forM names $ \n -> fmap (n,) <$> decodeFileStrict (d <> "/" <> n)

genEncoinsParams :: BuiltinByteString -> IO EncoinsProtocolParams
genEncoinsParams verifierPKH = (,,verifierPKH) <$> genTxOutRef <*> genTxOutRef

data EncoinsRequest
    = WalletRequest [Integer]
    | LedgerRequest [Integer]
    deriving Eq

genRequest :: Integer -> EncoinsMode -> Gen EncoinsRequest
genRequest maxAdaInToken mode = flip suchThat isValidRequest $ case mode of
    WalletMode -> do
        l <- choose (2, 5)
        fmap WalletRequest . replicateM l $ do
            ada <- choose (1, maxAdaInToken)
            b <- arbitrary
            pure $ if b then ada else negate ada
    LedgerMode -> do
        let genPieces = choose (0, 2) >>= (`replicateM` choose (1, maxAdaInToken))
        ms <- genPieces
        bs <- genPieces <&> map negate
        fmap LedgerRequest $ shuffle $ ms <> bs

isValidRequest :: EncoinsRequest -> Bool
isValidRequest eReq = case eReq of
        WalletRequest _ -> l >= 2 && l <= 5
        LedgerRequest _ -> l >= 2 && length toMint <= 2 && length toBurn <= 2 && sum req < 0
    where
        req = extractRequest eReq
        l = length req
        toMint = filter (>= 0) req
        toBurn = filter (<  0) req

instance Show EncoinsRequest where
    show req = let req' = extractRequest req in mconcat [show req', "(", show (sum req'), ")"]

extractRequest :: EncoinsRequest -> [Integer]
extractRequest = \case
    WalletRequest r -> r
    LedgerRequest r -> r

requestMode :: EncoinsRequest -> EncoinsMode
requestMode = \case
    WalletRequest _ -> WalletMode
    LedgerRequest _ -> LedgerMode

data TestEnv = TestEnv
    { teReq           :: [Integer]
    , teInputs        :: [(BuiltinByteString, MintingPolarity)]
    , teMint          :: [(TokenName, Integer)]
    , teEncoinsParams :: EncoinsProtocolParams
    , teRedeemer      :: EncoinsRedeemerOnChain
    , teLedgerAddr    :: Address
    , teChangeAddr    :: Address
    , teV             :: Integer
    , teFees          :: Integer
    , teDeposits      :: Integer
    }

-- Gen encoins redeemer and all other params necessary for testing
genTestEnv :: BuiltinByteString -> BuiltinByteString -> EncoinsRequest -> IO TestEnv
genTestEnv verifierPKH verifierPrvKey encoinsRequest = do
    let req = extractRequest encoinsRequest
    encoinsParams <- genEncoinsParams verifierPKH
    gammas <- replicateM (length req) randomIO
    randomness <- randomIO
    changeAddress <- genPubKeyAddress
    bulletproofSetup <- randomIO
    let mode                = requestMode encoinsRequest
        ledgerAddress       = ledgerValidatorAddress encoinsParams
        ps                  = map (\i -> if i < 0 then Burn else Mint) req
        secrets             = zipWith (\i g -> Secret g (toFieldElement i)) req gammas
        v                   = sum req
        fees                = 2 * protocolFee mode v
        par                 = (ledgerAddress, changeAddress, fees)
        bp                  = parseBulletproofParams $ sha2_256 $ toBytes par
        (_, inputs', proof) = bulletproof bulletproofSetup bp secrets ps randomness
        inputs              = toList . fromList $ map (\(Input g p) -> (fromGroupElement g, p)) inputs'
        signature           = ""
        red                 = (par, (v, inputs), proof, signature)
        redOnChain          = mkEncoinsRedeemerOnChain verifierPrvKey red
        deposits = case mode of
            WalletMode -> 0
            LedgerMode -> sum (polarityToInteger <$> ps)
    return TestEnv
        { teReq           = req
        , teInputs        = inputs
        , teMint          = sortBy (compare `on` fst) $ bimap TokenName polarityToInteger <$> inputs
        , teEncoinsParams = encoinsParams
        , teRedeemer      = redOnChain
        , teLedgerAddr    = ledgerAddress
        , teChangeAddr    = changeAddress
        , teV             = v
        , teFees          = fees
        , teDeposits      = deposits
        }