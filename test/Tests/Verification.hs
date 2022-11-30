{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Tests.Verification where

import           Control.Monad                    (mapM)
import           Data.Maybe                       (fromJust)
import           PlutusTx.Prelude                 hiding ((<$>), mapM)
import           Prelude                          (IO, print, unzip, (<$>))
import qualified Prelude                          as Haskell
import           Test.QuickCheck                  (quickCheck, Arbitrary (..))

import           Crypto                           (T1, Zp (..))
import           ENCOINS.Core.Bulletproofs.Prove  (bulletproof)
import           ENCOINS.Core.OnChain             (bulletproofSetup)
import           ENCOINS.Core.BaseTypes
import           ENCOINS.Core.Bulletproofs.Types  (Secret(..), Randomness (Randomness), Proof (..), Input (..), BulletproofSetup (..), BulletproofParams, Secrets)
import           ENCOINS.Core.Bulletproofs.Utils  (challenge, powers, withPolarity, powersOfZ, fromBits, toBits, padBits, polyProduct)
import           ENCOINS.Core.Bulletproofs.Verify (verify)


data TestVerification = TestVerification BulletproofSetup BulletproofParams Secrets [MintingPolarity] Randomness
    deriving (Haskell.Eq, Haskell.Show)

instance Arbitrary TestVerification where
    arbitrary = do
        let m = 10
        bs <- arbitrary
        bp <- arbitrary
        secrets <- mapM (const arbitrary) [1..m]
        mps     <- mapM (const arbitrary) [1..m]
        TestVerification bs bp secrets mps <$> arbitrary

prop_verification :: TestVerification -> Bool
prop_verification (TestVerification bs bp secrets mps r) = verify bs bp val ins proof
    where (val, ins, proof) = bulletproof bs bp secrets mps r