{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Data.Maybe                       (fromJust)
import           PlutusTx.Prelude
import           Prelude                          (IO, print, unzip)
import qualified Prelude                          as Haskell
import           Test.QuickCheck                  (quickCheck)

import           Crypto                           (T1, Zp (..))
import           ENCOINS.Core.Bulletproofs.Prove  (bulletproof)
import           ENCOINS.Core.OnChain             (bulletproofSetup)
import           ENCOINS.Core.BaseTypes
import           ENCOINS.Core.Bulletproofs.Types  (Secret(..), Randomness (Randomness), Proof (..), Input (..), BulletproofSetup (..))
import           ENCOINS.Core.Bulletproofs.Utils  (challenge, powers, withPolarity, powersOfZ, fromBits, toBits, padBits, polyProduct)
import           ENCOINS.Core.Bulletproofs.Verify (verify)


main :: IO ()
main = do
    let params = fromJust $ toGroupElement emptyByteString
        h = groupGenerator
        g = groupExp groupGenerator (toFieldElement 2)
        hs = map (groupExp groupGenerator . toFieldElement) [3..(100+2)]
        gs = map (groupExp groupGenerator . toFieldElement) [(100+3)..(2*100+2)]
        gammas = [F 1231]
        (vs, ps) = ([F 245], [Mint])
        secret = zipWith Secret gammas vs
        tau1   = F 7126519
        tau2   = F 1568502398532986345
        alpha  = F 8786
        rho    = F 78315421
        sL     = map F [1..10]
        sR     = map F [351..360]
        r      = Randomness alpha sL sR rho tau1 tau2
        (v, ins, proof) = bulletproof bulletproofSetup params secret ps r
        Proof commitA commitS commitT1 commitT2 taux mu lx rx tHat = proof
    print $ verify bulletproofSetup params 245 ins proof