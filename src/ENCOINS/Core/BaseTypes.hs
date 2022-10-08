{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module ENCOINS.Core.BaseTypes where

import           PlutusTx         (makeIsDataIndexed)
import           PlutusTx.Prelude
import           Prelude          (undefined)
import qualified Prelude          as Haskell


type FieldElement = BuiltinByteString

-- TODO: implement this
fieldZero :: FieldElement
fieldZero = ""

-- TODO: implement this
fieldOne :: FieldElement
fieldOne = ""

-- TODO: implement this
fieldTwo :: FieldElement
fieldTwo = ""

-- TODO: implement this
fieldAdd :: FieldElement -> FieldElement -> FieldElement
fieldAdd e1 _ = e1

-- TODO: implement this
fieldSub :: FieldElement -> FieldElement -> FieldElement
fieldSub e1 _ = e1

-- TODO: implement this
fieldNegate :: FieldElement -> FieldElement
fieldNegate e = e

-- TODO: implement this
fieldMul :: FieldElement -> FieldElement -> FieldElement
fieldMul e1 _ = e1

-- TODO: implement this
fieldInverse :: FieldElement -> FieldElement
fieldInverse e = e


-- TODO: implement this
{-# INLINABLE toFieldElement #-}
toFieldElement :: Integer -> FieldElement
toFieldElement = undefined

-- TODO: implement this
{-# INLINABLE fromFieldElement #-}
fromFieldElement :: FieldElement -> Integer
fromFieldElement = undefined

type GroupElement = BuiltinByteString

groupIdentity :: BuiltinByteString
groupIdentity = ""

-- TODO: implement this
groupMul :: GroupElement -> GroupElement -> GroupElement
groupMul g _ = g

-- TODO: implement this
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