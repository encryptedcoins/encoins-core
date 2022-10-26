{-# LANGUAGE DataKinds                  #-}
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

import           PlutusTx               (unstableMakeIsData)
import           PlutusTx.Prelude       (Integer)
import qualified Prelude                as Haskell

import           Crypto                 (Q, Zp)
import           ENCOINS.Core.BaseTypes (GroupElement, FieldElement, MintingPolarity)

-- NOTE: demo implementation
-- TODO: remove this later
unstableMakeIsData ''Q
unstableMakeIsData ''Zp

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
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''Secret

type Secrets = [Secret]

data Randomness = Randomness FieldElement [FieldElement] [FieldElement] FieldElement FieldElement FieldElement
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''Randomness

data Input = Input
    {
        inputCommit   :: GroupElement,
        inputPolarity :: MintingPolarity
    }
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''Input

type Inputs = [Input]

data Proof = Proof GroupElement GroupElement GroupElement GroupElement FieldElement FieldElement FieldElement [FieldElement] [FieldElement]
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''Proof