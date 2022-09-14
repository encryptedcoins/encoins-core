{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module ENCOINS.Core.Types where

import           PlutusTx.Prelude

type GroupElement = BuiltinByteString

type FieldElement = BuiltinByteString

-- TODO: implement this
fromFieldElement :: FieldElement -> Integer
fromFieldElement _ = 0