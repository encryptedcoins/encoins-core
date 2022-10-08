{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.Bulletproofs.Utils where

import           PlutusTx.Prelude
import           Prelude                 ((^))

import           ENCOINS.Core.BaseTypes
import           Utils.Prelude           (drop)

----------------------------------- Challenge ---------------------------------------

-- TODO: implement this
{-# INLINABLE challenge #-}
challenge :: GroupElement -> GroupElement -> (FieldElement, FieldElement)
challenge _ _ = (fieldZero, fieldZero)

------------------------------------- Delta -----------------------------------------

-- TODO: implement this
{-# INLINABLE delta #-}
delta :: Integer -> FieldElement -> FieldElement -> FieldElement
delta _ y _ = y

----------------------------------- Polynomials -------------------------------------

type Poly = ([FieldElement], [FieldElement])

-- TODO: implement this
polyProduct :: Poly -> Poly -> (FieldElement, FieldElement, FieldElement)
polyProduct _ _ = (fieldZero, fieldZero, fieldZero)

polyEvaluate :: Poly -> FieldElement -> [FieldElement]
polyEvaluate p x = zipWith fieldAdd (fst p) (map (`fieldMul` x) (snd p))

----------------------------------- Conversions -------------------------------------

-- TODO: implement this
toBits :: FieldElement -> [Integer]
toBits _ = []

-- TODO: implement this
fromBits :: [Integer] -> [FieldElement]
fromBits _ = []

----------------------------------- Arithmetics -------------------------------------

{-# INLINABLE powers #-}
powers :: FieldElement -> Integer -> [FieldElement]
powers _ 1 = [fieldOne]
powers e n = map (fieldMul e) $ powers e (n-1)

{-# INLINABLE powersOfZ #-}
powersOfZ :: FieldElement -> Integer -> ([FieldElement], FieldElement)
powersOfZ z m = (\lst -> (take m lst, lst !! m)) $ drop 2 $ powers z (m+3)

------------------------------------- Polarity --------------------------------------

polarity :: Integer -> FieldElement -> (FieldElement, MintingPolarity)
polarity n v
    | fromFieldElement v               < 2^n = (v, Mint)
    | fromFieldElement (fieldNegate v) < 2^n = (fieldNegate v, Burn)
    | otherwise = error ()

{-# INLINABLE withPolarity #-}
withPolarity :: MintingPolarity -> FieldElement -> FieldElement
withPolarity p v = if p == Mint then v else fieldNegate v