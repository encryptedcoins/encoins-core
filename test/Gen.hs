{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Gen where

import           Control.Monad          (replicateM)
import           Test.QuickCheck        (Arbitrary (..), choose)

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
        pure $ MintRequest $ map abs req

newtype BurnRequest = BurnRequest [Integer]
    deriving (Show, Eq)
    deriving newtype EncoinsRequest

instance Arbitrary BurnRequest where
    arbitrary = do
        MintRequest req <- arbitrary
        pure $ BurnRequest $ map negate req

newtype MixedRequest = MixedRequest [Integer]
    deriving (Show, Eq)
    deriving newtype EncoinsRequest

instance Arbitrary MixedRequest where
    arbitrary = fmap MixedRequest $ choose (1,5) >>= (`replicateM` choose (-100, 100))