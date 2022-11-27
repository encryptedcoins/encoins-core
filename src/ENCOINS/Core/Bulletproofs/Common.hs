{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.Bulletproofs.Common where

import           PlutusTx.Prelude
import           Prelude                          (Show)

import           ENCOINS.Core.BaseTypes
import           ENCOINS.Core.Bulletproofs.Types  (BulletproofSetup (..), BulletproofParams)
import           ENCOINS.Core.Bulletproofs.Utils

data CommonPart = CommonPart FieldElement FieldElement [FieldElement] [FieldElement] [FieldElement] [GroupElement]
    deriving Show

commonPart :: BulletproofSetup -> BulletproofParams -> [MintingPolarity] -> (GroupElement, GroupElement) -> CommonPart
commonPart (BulletproofSetup _ _ hs _ n) bp ps (commitA, commitS) = CommonPart z z' ys zs lam hs'
    where
        m        = length ps
        twos     = powers (F 2) n
        (y, z)   = challenge [commitA, commitS, bp]
        ys       = powers y (n * m)
        (zs, z') = powersOfZ z m
        lam1     = map (* z) ys
        lam2     = concat $ zipWith (\pj zj -> map (\a -> (a * zj) + (withPolarity pj a *  z')) twos) ps zs
        lam      = zipWith (+) lam1 lam2
        hs'      = zipWith groupExp hs (map inv ys)