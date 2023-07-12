{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Gen where

import           Control.Monad              (replicateM)
import           ENCOINS.Core.OnChain       (EncoinsProtocolParams)
import           Plutus.V2.Ledger.Api       (BuiltinByteString)
import           PlutusAppsExtra.Test.Utils (genTxOutRef)
import           Test.QuickCheck            (Arbitrary (..), choose, generate)

genEncoinsParams :: BuiltinByteString -> IO EncoinsProtocolParams
genEncoinsParams verifierPKH = (,,verifierPKH) <$> genTxOutRef <*> genTxOutRef

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
    arbitrary = fmap MixedRequest $ do
        l <- choose (1,5)
        replicateM l $ do
            ada <- choose (1, 100)
            b <- arbitrary
            pure $ if b then ada else negate ada