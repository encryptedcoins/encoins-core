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
import           Utils.ByteString        (byteStringToInteger)

----------------------------------- Challenge ---------------------------------------

{-# INLINABLE challenge #-}
challenge :: GroupElement -> GroupElement -> (FieldElement, FieldElement)
challenge g1 g2 = (f1, f2)
    where
        bs = sha2_256 $ fromGroupElement g1 `appendByteString` fromGroupElement g2
        f1 = toFieldElement $ byteStringToInteger bs
        f2 = toFieldElement $ byteStringToInteger $ sha2_256 bs

----------------------------------- Polynomials -------------------------------------

type Poly = ([FieldElement], [FieldElement])

polyProduct :: Poly -> Poly -> (FieldElement, FieldElement, FieldElement)
polyProduct (l0, l1) (r0, r1) = (c0, c1, c2)
    where
        c0 = sum $ zipWith (*) l0 r0
        c1 = sum $ zipWith (*) l1 r0 ++ zipWith (*) l0 r1
        c2 = sum $ zipWith (*) l1 r1

polyEvaluate :: Poly -> FieldElement -> [FieldElement]
polyEvaluate p x = zipWith (+) (fst p) (map (* x) (snd p))

----------------------------------- Conversions -------------------------------------

toBits :: FieldElement -> [Integer]
toBits (F a) = r : if q > 0 then toBits (F q) else []
    where (q, r) = divMod a 2

fromBits :: [Integer] -> [FieldElement]
fromBits = map F

----------------------------------- Arithmetics -------------------------------------

{-# INLINABLE powers #-}
powers :: FieldElement -> Integer -> [FieldElement]
powers _ 1 = [one]
powers e n = map (* e) $ powers e (n-1)

{-# INLINABLE powersOfZ #-}
powersOfZ :: FieldElement -> Integer -> ([FieldElement], FieldElement)
powersOfZ z m = (\lst -> (take m lst, lst !! m)) $ drop 2 $ powers z (m+3)

------------------------------------- Polarity --------------------------------------

polarity :: Integer -> FieldElement -> (FieldElement, MintingPolarity)
polarity n v
    | fromFieldElement v          < 2^n = (v, Mint)
    | fromFieldElement (negate v) < 2^n = (negate v, Burn)
    | otherwise = error ()

{-# INLINABLE withPolarity #-}
withPolarity :: MintingPolarity -> FieldElement -> FieldElement
withPolarity p v = if p == Mint then v else negate v