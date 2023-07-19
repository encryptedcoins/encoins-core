{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal where

import           Cardano.Node.Emulator.Params ()
import           Data.Aeson                   (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing            (aesonPrefix, snakeCase)
import           GHC.Generics                 (Generic)
import           Ledger

data TestConfig = TestConfig
    { tcProtocolParamsFile :: FilePath
    , tcVerifierPkhFile    :: FilePath
    , tcVerifierPrvKeyFile :: FilePath
    , tcNetworkId          :: NetworkId
    -- , tcBlockChainEnvs     :: [BlockchainEnv]
    } deriving (Show, Generic)

instance FromJSON TestConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data BlockchainEnv = BlockchainEnv
    { beMaxWalletUtxos       :: Integer
    , beMaxAdaInWalletUtxo   :: Integer
    , beMaxLedgerUtxos       :: Integer
    , beMaxAdaInLedgerUtxo   :: Integer
    , beMaxLedgerExtraTokens :: Integer
    } deriving (Show, Generic)

instance FromJSON BlockchainEnv where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
