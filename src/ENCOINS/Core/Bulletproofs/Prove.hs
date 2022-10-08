{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.Bulletproofs.Prove where

import           PlutusTx.Prelude
import           Prelude                          (unzip)

import           ENCOINS.Core.BaseTypes
import           ENCOINS.Core.Bulletproofs.Types
import           ENCOINS.Core.Bulletproofs.Utils

-- TODO: add data correctness checks
bulletproof :: BulletproofSetup -> Secrets -> Randomness -> (Inputs, Proof, Integer)
bulletproof (BulletproofSetup h g hs gs n) secrets (Randomness alpha sL sR rho tau1 tau2) = (zipWith Input commitVs ps,
        Proof commitA commitS commitT1 commitT2 taux mu tHat lx rx, val)
    where
        m        = length secrets
        gammas   = map secretGamma secrets
        (vs, ps) = unzip $ map (polarity n . secretV) secrets
        val      = fromFieldElement $ foldl (+) zero (map secretV secrets)
        aL       = concatMap (fromBits . toBits) vs
        aR       = concatMap (fromBits . map (\q -> q - 1) . toBits) vs
        commitA  = foldl groupMul groupIdentity (groupExp h alpha : zipWith groupExp gs aL ++ zipWith groupExp hs aR)
        commitS  = foldl groupMul groupIdentity (groupExp h rho : zipWith groupExp gs sL ++ zipWith groupExp hs sR)
        (y, z)   = challenge commitA commitS
        (zs, z') = powersOfZ z m
        l        = (map (\a -> a - z) aL, sL)
        r        = (zipWith (+) (zipWith (*) (powers y (n * m)) (map (+ z) aR))
                    (concatMap (\zj -> zipWith (\a p -> (a * zj) + (withPolarity p a * z')) (powers (F 2) n) ps) zs),
                    zipWith (*) (powers y (n * m)) sR)
        (_, t1, t2) = polyProduct l r
        commitT1 = groupMul (groupExp g t1) (groupExp g tau1)
        commitT2 = groupMul (groupExp g t2) (groupExp g tau2)
        (x, _)   = challenge commitT1 commitT2
        x2       = x * x
        lx       = polyEvaluate l x
        rx       = polyEvaluate r x
        tHat     = foldl (+) zero $ zipWith (*) lx rx
        taux     = (tau2 * x2) + (tau1 * x) + foldl (+) zero (zipWith (*) zs gammas)
        mu       = alpha + (rho * x)
        commitVs = zipWith groupMul (map (groupExp h) gammas) (map (groupExp g) vs)