{-# LANGUAGE DataKinds                  #-}
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


module ENCOINS.Core.BaseTypes where

import           PlutusTx         (makeIsDataIndexed, unstableMakeIsData)
import           PlutusTx.Prelude
import qualified Prelude          as Haskell

import           Crypto           (T1, toZp, fromZp, addJ, mulJ)
import           Utils.ByteString (byteStringToInteger, integerToByteString)


newtype FieldElement = F Integer
    deriving (Eq, Haskell.Eq, Haskell.Show)

unstableMakeIsData ''FieldElement

{-# INLINABLE toFieldElement #-}
toFieldElement :: Integer -> FieldElement
toFieldElement = F

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

-- NOTE: demo implementation
-- TODO: implement this
type GroupElement = (T1, T1, T1)

instance Eq GroupElement where
    (==) (x1, x2, x3) (y1, y2, y3) = x1 == y1 && x2 == y2 && x3 == y3

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE toGroupElement #-}
toGroupElement :: BuiltinByteString -> GroupElement
toGroupElement bs = (x, y, z)
    where
        x = toZp $ byteStringToInteger $ takeByteString 48 bs
        y = toZp $ byteStringToInteger $ takeByteString 48 $ dropByteString 48 bs
        z = toZp $ byteStringToInteger $ takeByteString 48 $ dropByteString 96 bs

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE fromGroupElement #-}
fromGroupElement :: GroupElement -> BuiltinByteString
fromGroupElement (x, y, z) = foldl appendByteString emptyByteString $ map (integerToByteString . fromZp) [x, y, z]

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE groupIdentity #-}
groupIdentity :: GroupElement
groupIdentity = (one, one, zero)

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE groupMul #-}
groupMul :: GroupElement -> GroupElement -> GroupElement
groupMul = addJ

-- NOTE: demo implementation
-- TODO: implement this
{-# INLINABLE groupExp #-}
groupExp :: GroupElement -> FieldElement -> GroupElement
groupExp g = mulJ g . fromFieldElement

data MintingPolarity = Mint | Burn
    deriving (Haskell.Eq, Haskell.Show)

instance Eq MintingPolarity where
    Mint == Mint = True
    Burn == Burn = True
    _ == _       = False

makeIsDataIndexed ''MintingPolarity [('Mint,0),('Burn,1)]

{-# INLINABLE polarityToInteger #-}
polarityToInteger :: MintingPolarity -> Integer
polarityToInteger Mint = 1
polarityToInteger Burn = -1