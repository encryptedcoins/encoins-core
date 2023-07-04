{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Gen where

import           Control.Monad        (replicateM)
import           ENCOINS.Core.OnChain (EncoinsProtocolParams)
import           Plutus.V2.Ledger.Api (BuiltinByteString)
import           Test.QuickCheck      (Arbitrary (..), choose, generate)

genEncoinsParams :: BuiltinByteString -> IO EncoinsProtocolParams
genEncoinsParams verifierPKH = generate $ (,,verifierPKH) <$> arbitrary <*> arbitrary
class EncoinsRequest a where
    extractRequest :: a -> [Integer]

instance EncoinsRequest [Integer] where
    extractRequest = id

newtype MintRequest = MintRequest [Integer]
    deriving (Show, Eq)
    deriving newtype EncoinsRequest

instance Arbitrary MintRequest where
    arbitrary = do
        MixedRequest req <- arbitrary
        pure $ MintRequest $ map abs $ filter (/= 0) req

newtype BurnRequest = BurnRequest [Integer]
    deriving (Show, Eq)
    deriving newtype EncoinsRequest

instance Arbitrary BurnRequest where
    arbitrary = do
        MintRequest req <- arbitrary
        pure $ BurnRequest $ map negate $ filter (/= 0) req

newtype MixedRequest = MixedRequest [Integer]
    deriving (Show, Eq)
    deriving newtype EncoinsRequest

instance Arbitrary MixedRequest where
    arbitrary = fmap MixedRequest $ choose (1,5) >>= (`replicateM` choose (-100, 100))