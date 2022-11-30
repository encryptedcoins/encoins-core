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
import           ENCOINS.Core.Bulletproofs.Common
import           ENCOINS.Core.Bulletproofs.Utils  (challenge, powers, polarityToInteger)
import           ENCOINS.Core.Bulletproofs.Types

{-# INLINABLE verify #-}
verify :: BulletproofSetup -> BulletproofParams -> Integer -> Inputs -> Proof -> Bool
verify bs@(BulletproofSetup h g _ gs n) bp val inputs (Proof commitA commitS commitT1 commitT2 taux mu lx rx tHat) = cond1 && cond2 && cond3
    where
        m        = length inputs
        ps       = map inputPolarity inputs
        commitVs = map inputCommit inputs

        CommonPart z z' ys zs lam hs' = commonPart bs bp ps (commitA, commitS)

        (x, _)   = challenge [commitT1, commitT2]
        x2       = x * x

        commitP  = foldl groupMul groupIdentity (commitA : groupExp commitS x : map (`groupExp` negate z) (take (n*m) gs) ++ zipWith groupExp hs' lam)
        twos     = powers (F 2) n
        s        = sum twos
        psSum    = F $ sum $ map polarityToInteger ps
        delta    = ((z - z*z) * sum ys) - z * s * sum zs - z * s * z' * psSum + z' * toFieldElement val

        cond1    = groupExp g tHat `groupMul` groupExp h taux ==
            groupExp g delta
            `groupMul` foldl groupMul groupIdentity (zipWith groupExp commitVs zs)
            `groupMul` groupExp commitT1 x
            `groupMul` groupExp commitT2 x2
        cond2    = commitP == foldl groupMul groupIdentity (groupExp h mu : zipWith groupExp gs lx ++ zipWith groupExp hs' rx)
        cond3    = tHat == foldl (+) zero (zipWith (*) lx rx)

