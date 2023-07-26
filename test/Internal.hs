{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Internal where

import           Control.Monad                (forM, replicateM)
import           Data.Aeson                   (FromJSON (..), decodeFileStrict, genericParseJSON)
import           Data.Aeson.Casing            (aesonPrefix, snakeCase)
import           Data.Default                 (Default (..))
import           Data.Functor                 ((<&>))
import           Data.List                    (isPrefixOf)
import           Data.Maybe                   (catMaybes)
import           ENCOINS.Core.OffChain        (EncoinsMode (..))
import           ENCOINS.Core.OnChain         (EncoinsProtocolParams)
import           GHC.Generics                 (Generic)
import           Ledger                       (NetworkId)
import           Plutus.V2.Ledger.Api         (BuiltinByteString)
import           PlutusAppsExtra.Test.Utils   (genTxOutRef)
import           System.Directory             (listDirectory)
import           Test.QuickCheck              (Arbitrary (..), Gen, choose, shuffle, suchThat)

data TestConfig = TestConfig
    { tcProtocolParamsFile :: FilePath
    , tcVerifierPkhFile    :: FilePath
    , tcVerifierPrvKeyFile :: FilePath
    , tcNetworkId          :: NetworkId
    } deriving (Show, Generic)

instance FromJSON TestConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data TestSpecification = TestSpecification
    { tsWalletUtxosAmt      :: Int
    , tsAdaInWalletUtxo     :: Integer
    , tsLedgerUtxosAmt      :: Int
    , tsMaxAdaInSingleToken :: Integer
    , tsShouldFail          :: Bool
    } deriving (Show, Generic)

instance Default TestSpecification where
    def = TestSpecification 0 0 0 1000 False

instance FromJSON TestSpecification where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

getSpecifications :: IO [(String, TestSpecification)]
getSpecifications = (("no specification", def) :) <$> do
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
        LedgerRequest _ -> l >= 2 && length toMint <= 2 && length toBurn <= 2
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