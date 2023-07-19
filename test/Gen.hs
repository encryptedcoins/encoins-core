{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances #-}

module Gen where

import           Control.Monad              (replicateM)
import           ENCOINS.Core.OnChain       (EncoinsProtocolParams)
import           Plutus.V2.Ledger.Api       (BuiltinByteString, TxOutRef (..), TxId (..))
import           PlutusAppsExtra.Test.Utils (genTxOutRef)
import           Test.QuickCheck            (Arbitrary (..), choose, generate)
import GHC.Exts (IsList)

genEncoinsParams :: BuiltinByteString -> IO EncoinsProtocolParams
genEncoinsParams verifierPKH = (,,verifierPKH) <$> genTxOutRef <*> genTxOutRef

class EncoinsRequest a where
    extractRequest :: a -> [Integer]

instance EncoinsRequest [Integer] where
    extractRequest = id

newtype MintRequest = MintRequest [Integer]
    deriving (Eq)
    deriving newtype (EncoinsRequest, IsList)

instance Arbitrary MintRequest where
    arbitrary = do
        MixedRequest req <- arbitrary
        pure $ MintRequest $ map abs req

newtype BurnRequest = BurnRequest [Integer]
    deriving (Eq)
    deriving newtype (EncoinsRequest, IsList)

instance Arbitrary BurnRequest where
    arbitrary = do
        MintRequest req <- arbitrary
        pure $ BurnRequest $ map negate req

newtype MixedRequest = MixedRequest [Integer]
    deriving (Eq)
    deriving newtype (EncoinsRequest, IsList)

instance Arbitrary MixedRequest where
    arbitrary = fmap MixedRequest $ do
        l <- choose (1, 5)
        replicateM l $ do
            ada <- choose (1, 100)
            b <- arbitrary
            pure $ if b then ada else negate ada

instance {-# OVERLAPPABLE #-} EncoinsRequest a => Show a where
    show req = show (extractRequest req) <> "(" <> show (sum $ extractRequest req) <> ")"