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

type GroupElement = BuiltinByteString

-- TODO: implement this
{-# INLINABLE toGroupElement #-}
toGroupElement :: BuiltinByteString -> GroupElement
toGroupElement _ = ""

-- TODO: implement this
{-# INLINABLE fromGroupElement #-}
fromGroupElement :: GroupElement -> BuiltinByteString
fromGroupElement _ = ""

-- TODO: implement this
{-# INLINABLE groupIdentity #-}
groupIdentity :: BuiltinByteString
groupIdentity = ""

-- TODO: implement this
{-# INLINABLE groupMul #-}
groupMul :: GroupElement -> GroupElement -> GroupElement
groupMul g _ = g

-- TODO: implement this
{-# INLINABLE groupExp #-}
groupExp :: GroupElement -> FieldElement -> GroupElement
groupExp g _ = g

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