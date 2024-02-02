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

import           ENCOINS.Core.V1.OffChain.Modes (EncoinsMode (..))
import qualified Plutus.Script.Utils.Ada        as P
import qualified Plutus.V2.Ledger.Api           as P
import           PlutusTx.Prelude               hiding (mapM, (<$>), (<>))

protocolFeeWalletMode :: Integer -> Integer
protocolFeeWalletMode v
    | v >= 0    = 0
    | otherwise = max 2 $ negate v `divide` 200

protocolFeeLedgerMode :: Integer -> Integer
protocolFeeLedgerMode v
    | v >= 0    = 2
    | otherwise = max 2 $ negate v `divide` 200

protocolFee :: EncoinsMode -> Integer -> Integer
protocolFee mode v = case mode of
    WalletMode -> protocolFeeWalletMode v
    LedgerMode -> protocolFeeLedgerMode v

treasuryFee :: EncoinsMode -> Integer -> Integer
treasuryFee mode v = if protocolFee mode v < 4 then 0 else protocolFee mode v `divide` 4

protocolFeeValue :: EncoinsMode -> Integer -> P.Value
protocolFeeValue mode v = P.lovelaceValueOf . (* 1_000_000) $ protocolFee mode v

treasuryFeeValue :: EncoinsMode -> Integer -> P.Value
treasuryFeeValue mode v = P.lovelaceValueOf . (* 1_000_000) $ treasuryFee mode v