{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module ENCOINS.Core.Bulletproofs where

import           Plutus.Contract.StateMachine.MintingPolarity   (MintingPolarity (..))
import           PlutusTx.Prelude                               (Bool (..), Integer, emptyByteString)

import           ENCOINS.Core.Types                             (GroupElement, FieldElement)


type Input = ([(GroupElement, MintingPolarity)], FieldElement)

{-# INLINABLE polarityToInteger #-}
polarityToInteger :: MintingPolarity -> Integer
polarityToInteger Mint = 1
polarityToInteger Burn = -1

type Proof = (GroupElement, FieldElement) -- TODO: correct these type

fakeProof :: Proof
fakeProof = (emptyByteString, emptyByteString)

-- TODO: implement this
{-# INLINABLE verify #-}
verify :: Input -> Proof -> Bool
verify _ _ = True