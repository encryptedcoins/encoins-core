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
import           Tests.Verification               (prop_verification)

main :: IO ()
main = do
    quickCheck prop_verification
