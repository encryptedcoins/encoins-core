{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module ENCOINS.Core.Bulletproofs.Types where

import           Control.Monad.Extra                (mapM)
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           PlutusTx                           (unstableMakeIsData)
import           PlutusTx.Prelude                   hiding ((<$>), mapM)
import           Prelude                            ((^), (<$>))
import qualified Prelude                            as Haskell
import           Test.QuickCheck                    (Arbitrary(..))

import           ENCOINS.Core.BaseTypes             (GroupElement, FieldElement (..), MintingPolarity)


-- TODO: remove hardcoded constants in these module

data BulletproofSetup = BulletproofSetup GroupElement GroupElement [GroupElement] [GroupElement] Integer
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''BulletproofSetup

instance Arbitrary BulletproofSetup where
    arbitrary = do
        let n = 10
            m = 10
        h  <- arbitrary
        g  <- arbitrary
        hs <- mapM (const arbitrary) [1..(n*m)]
        gs <- mapM (const arbitrary) [1..(n*m)]
        return $ BulletproofSetup h g hs gs n

-- A type that encodes public input parameters: deposit/withdrawal address public key and validity interval
type BulletproofParams = GroupElement

data Secret = Secret
    {
        secretGamma :: FieldElement,
        secretV     :: FieldElement
    }
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''Secret

instance Arbitrary Secret where
    arbitrary = do
        let n = 10 :: Integer
        gamma <- arbitrary
        v     <- F . (`modulo` (2^n)) <$> arbitrary
        return $ Secret gamma v

type Secrets = [Secret]

data Randomness = Randomness FieldElement [FieldElement] [FieldElement] FieldElement FieldElement FieldElement
    deriving (Haskell.Eq, Haskell.Show, Generic)

unstableMakeIsData ''Randomness

instance Arbitrary Randomness where
    arbitrary = do
        let n = 10 :: Integer
            m = 10 :: Integer
        alpha <- arbitrary
        sL    <- mapM (const arbitrary) [1..(n*m)]
        sR    <- mapM (const arbitrary) [1..(n*m)]
        rho   <- arbitrary
        tau1  <- arbitrary
        Randomness alpha sL sR rho tau1 <$> arbitrary

data Input = Input
    {
        inputCommit   :: GroupElement,
        inputPolarity :: MintingPolarity
    }
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''Input

type Inputs = [Input]

data Proof = Proof GroupElement GroupElement GroupElement GroupElement FieldElement FieldElement [FieldElement] [FieldElement] FieldElement
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''Proof