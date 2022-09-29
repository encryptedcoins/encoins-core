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


module ENCOINS.Core.Types where

import           PlutusTx         (makeIsDataIndexed)
import           PlutusTx.Prelude
import           Prelude          (undefined)
import qualified Prelude          as Haskell


type GroupElement = BuiltinByteString

type FieldElement = BuiltinByteString

-- TODO: implement this
{-# INLINABLE toFieldElement #-}
toFieldElement :: Integer -> FieldElement
toFieldElement = undefined

-- TODO: implement this
{-# INLINABLE fromFieldElement #-}
fromFieldElement :: FieldElement -> Integer
fromFieldElement = undefined

data MintingPolarity = Mint | Burn
    deriving (Haskell.Eq, Haskell.Show)

makeIsDataIndexed ''MintingPolarity [('Mint,0),('Burn,1)]

instance Eq MintingPolarity where
    Mint == Mint = True
    Burn == Burn = True
    _ == _       = False

{-# INLINABLE polarityToInteger #-}
polarityToInteger :: MintingPolarity -> Integer
polarityToInteger Mint = 1
polarityToInteger Burn = -1