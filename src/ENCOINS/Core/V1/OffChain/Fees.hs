{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module ENCOINS.Core.V1.OffChain.Fees where

import           Ledger.Ada                     (lovelaceValueOf)
import           Ledger.Value                   (Value)
import           PlutusTx.Prelude               hiding (mapM, (<$>), (<>))

import           ENCOINS.Core.V1.OffChain.Modes (EncoinsMode(..))

protocolFeeWalletMode :: Integer -> Integer
protocolFeeWalletMode v
    | v >= 0    = 0
    | otherwise = max 2 $ v `divide` 200

protocolFeeLedgerMode :: Integer -> Integer
protocolFeeLedgerMode v
    | v >= 0    = 2
    | otherwise = max 2 $ v `divide` 200

protocolFee :: EncoinsMode -> Integer -> Integer
protocolFee mode v = case mode of
    WalletMode -> protocolFeeWalletMode v
    LedgerMode -> protocolFeeLedgerMode v

protocolFeeValue :: EncoinsMode -> Integer -> Value
protocolFeeValue mode v = lovelaceValueOf . (* 1_000_000) $ protocolFee mode v