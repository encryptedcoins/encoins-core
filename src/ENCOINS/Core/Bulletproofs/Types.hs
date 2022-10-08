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

import           ENCOINS.Core.BaseTypes (GroupElement, FieldElement, MintingPolarity)


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

data BulletproofSetup = BulletproofSetup GroupElement GroupElement [GroupElement] [GroupElement] Integer
    deriving (Haskell.Eq, Haskell.Show)

unstableMakeIsData ''BulletproofSetup

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