{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module ENCOINS.Core.V1.OffChain.Modes where

import           Data.Aeson                               (FromJSON, ToJSON)
import           GHC.Generics                             (Generic)
import           PlutusTx.Prelude                         hiding (mapM, (<$>), (<>))
import qualified Prelude                                  as Haskell

data EncoinsMode = WalletMode | LedgerMode
    deriving (Haskell.Show, Haskell.Read, Haskell.Eq, Generic, FromJSON, ToJSON)

instance Eq EncoinsMode where
    (==) = (Haskell.==)