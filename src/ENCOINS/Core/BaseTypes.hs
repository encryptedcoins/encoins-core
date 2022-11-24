{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module ENCOINS.Core.BaseTypes where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Bifunctor            (Bifunctor(..))
import           Data.Functor              ((<$>))
import           GHC.Generics              (Generic)
import           PlutusTx                  (makeIsDataIndexed, unstableMakeIsData)
import           PlutusTx.Prelude          hiding ((<$>))
import qualified Prelude                   as Haskell
import           System.Random             (Random (..), Uniform, UniformRange)
import           System.Random.Stateful    (Uniform(..), UniformRange (..))
import           Test.QuickCheck           (Arbitrary(..))

import           Crypto                    (T1, toZp, fromZp, addJ, mulJ, dblJ, fromXJ, CurvePoint (..), fromJ)
import           Utils.ByteString          (toBytes, byteStringToInteger)


newtype FieldElement = F Integer
    deriving (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Eq FieldElement where
    (==) (F a) (F b) = a == b

unstableMakeIsData ''FieldElement

{-# INLINABLE toFieldElement #-}
toFieldElement :: Integer -> FieldElement
toFieldElement a = F $ modulo a fieldPrime

{-# INLINABLE fromFieldElement #-}
fromFieldElement :: FieldElement -> Integer
fromFieldElement (F a) = a

{-# INLINABLE fieldPrime #-}
fieldPrime :: Integer
fieldPrime = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

instance AdditiveSemigroup FieldElement where
    {-# INLINABLE (+) #-}
    (+) (F a) (F b) = F $ modulo (a + b) fieldPrime

instance AdditiveGroup FieldElement where
    {-# INLINABLE (-) #-}
    (-) (F a) (F b) = F $ modulo (a - b) fieldPrime

instance AdditiveMonoid FieldElement where
    {-# INLINABLE zero #-}
    zero = F 0

instance MultiplicativeSemigroup FieldElement where
    {-# INLINABLE (*) #-}
    (*) (F a) (F b) = F $ modulo (a * b) fieldPrime

instance MultiplicativeMonoid FieldElement where
    {-# INLINABLE one #-}
    one = F 1

instance Semigroup FieldElement where
    {-# INLINABLE (<>) #-}
    (<>) = (*)

instance Monoid FieldElement where
    {-# INLINABLE mempty #-}
    mempty = one

instance Group FieldElement where
    {-# INLINABLE inv #-}
    inv (F a) = F (modulo (snd $ f (a, 1) (fieldPrime, 0)) fieldPrime)
      where
        f (x, y) (x', y')
                    | x' == zero = (x, y)
                    | otherwise  = f (x', y') (x - q * x', y - q * y')
          where q = divide x x'

instance Arbitrary FieldElement where
  {-# INLINABLE arbitrary #-}
  arbitrary = do
    n <- arbitrary
    return $ F $ modulo n fieldPrime

instance UniformRange FieldElement where
    uniformRM (F a, F b) g = F <$> uniformRM (a, b) g

instance Uniform FieldElement where
    uniformM = uniformRM (zero, F $ fieldPrime - 1)

instance Random FieldElement where
    randomR (F a, F b) g = first F $ randomR (a, b) g
    randomRs (F a, F b)  = map F . randomRs (a, b)
    random               = randomR (zero, F $ fieldPrime - 1)
    randoms              = map F . randoms

-- NOTE: demo implementation
-- TODO: implement this
type GroupElement = (T1, T1, T1)

instance Eq GroupElement where
    (==) (x1, x2, x3) (y1, y2, y3) = x1 == y1 && x2 == y2 && x3 == y3

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE toGroupElement #-}
toGroupElement :: BuiltinByteString -> Maybe GroupElement
toGroupElement bs = if n == q then Just (one, one, zero) else fromXJ $ toZp n
    where n = byteStringToInteger bs
          q = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE fromGroupElement #-}
fromGroupElement :: GroupElement -> BuiltinByteString
fromGroupElement g = toBytes n
    where n = case fromJ g of
            CP x _ -> fromZp x
            O      -> 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE groupIdentity #-}
groupIdentity :: GroupElement
groupIdentity = (one, one, zero)

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE groupGenerator #-}
groupGenerator :: GroupElement
groupGenerator = (toZp 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb,
    toZp 0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1, one)

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE groupMul #-}
groupMul :: GroupElement -> GroupElement -> GroupElement
groupMul g1 g2
    | g1 == g2  = dblJ g1
    | otherwise = addJ g1 g2

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE groupExp #-}
groupExp :: GroupElement -> FieldElement -> GroupElement
groupExp g = mulJ g . fromFieldElement

data MintingPolarity = Mint | Burn
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq MintingPolarity where
    Mint == Mint = True
    Burn == Burn = True
    _ == _       = False

makeIsDataIndexed ''MintingPolarity [('Mint,0),('Burn,1)]

{-# INLINABLE polarityToInteger #-}
polarityToInteger :: MintingPolarity -> Integer
polarityToInteger Mint = 1
polarityToInteger Burn = -1