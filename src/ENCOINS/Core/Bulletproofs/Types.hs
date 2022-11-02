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

import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           PlutusTx                           (unstableMakeIsData)
import           PlutusTx.Prelude                   (Integer)
import qualified Prelude                            as Haskell
import           Test.QuickCheck                    (Arbitrary(..))
import           Test.QuickCheck.Arbitrary.Generic  (genericArbitrary)

import           ENCOINS.Core.BaseTypes             (GroupElement, FieldElement, MintingPolarity)


data BulletproofSetup = BulletproofSetup GroupElement GroupElement [GroupElement] [GroupElement] Integer
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''BulletproofSetup

-- A type that encodes public input parameters: deposit/withdrawal address public key and validity interval
type BulletproofParams = GroupElement

data Secret = Secret
    {
        secretGamma :: FieldElement,
        secretV     :: FieldElement
    }
    deriving (Haskell.Eq, Haskell.Show, Generic)

instance Arbitrary Secret where
    arbitrary = genericArbitrary

unstableMakeIsData ''Secret

type Secrets = [Secret]

data Randomness = Randomness FieldElement [FieldElement] [FieldElement] FieldElement FieldElement FieldElement
    deriving (Haskell.Eq, Haskell.Show, Generic)

instance Arbitrary Randomness where
    arbitrary = genericArbitrary

unstableMakeIsData ''Randomness

data Input = Input
    {
        inputCommit   :: GroupElement,
        inputPolarity :: MintingPolarity
    }
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''Input

type Inputs = [Input]

data Proof = Proof GroupElement GroupElement GroupElement GroupElement FieldElement FieldElement FieldElement [FieldElement] [FieldElement]
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''Proof