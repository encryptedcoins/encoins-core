{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.Bulletproofs.Verify where

import           PlutusTx.Prelude

import           ENCOINS.Core.BaseTypes
import           ENCOINS.Core.Bulletproofs.Utils
import           ENCOINS.Core.Bulletproofs.Types

{-# INLINABLE verify #-}
verify :: BulletproofSetup -> BulletproofParams -> Integer -> Inputs -> Proof -> Bool
verify (BulletproofSetup h g hs gs n) bp val inputs (Proof commitA commitS commitT1 commitT2 taux mu tHat lx rx) = cond1 && cond2 && cond3
    where
        commitVs = map inputCommit inputs
        ps       = map inputPolarity inputs
        m        = length commitVs
        (y, z)   = challenge [commitA, commitS, bp]
        (x, _)   = challenge [commitT1, commitT2]
        (zs, z') = powersOfZ z m
        twos     = powers (F 2) n
        x2       = x * x
        ys       = powers y (n * m)
        hs'      = zipWith groupExp hs (map inv ys)
        lam1     = map (* z) ys
        lam2     = concatMap (\zj -> zipWith (\p a -> (a * zj) + (withPolarity p a *  z')) ps twos) zs
        lam      = zipWith (+) lam1 lam2
        commitP  = foldl groupMul groupIdentity (commitA : groupExp commitS x : map (`groupExp` negate z) gs ++ zipWith groupExp hs' lam)
        s        = sum twos
        psSum    = F $ sum $ map polarityToInteger ps
        delta    = ((z - z*z) * sum ys) - sum (map (* s) zs) - (z * z' * s * psSum) + z' * F val

        cond1    = groupExp g tHat `groupMul` groupExp h taux ==
            groupExp g delta
            `groupMul` foldl groupMul groupIdentity (zipWith groupExp commitVs zs)
            `groupMul` groupExp commitT1 x
            `groupMul` groupExp commitT2 x2
        cond2    = commitP == foldl groupMul groupIdentity (groupExp h mu : zipWith groupExp gs lx ++ zipWith groupExp hs' rx)
        cond3    = tHat == foldl (+) zero (zipWith (*) lx rx)

