{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Cardano.Api                      (NetworkId (..), NetworkMagic (..), SerialiseAddress (..), writeFileJSON)
import           Data.Aeson                       (ToJSON(..))
import           Data.Either                      (fromRight)
import           Data.List                        (intercalate)
import           Data.Maybe                       (fromJust)
import           Data.Text                        (pack)
import           Ledger                           (TxOutRef(..), TxId (..))
import           Ledger.Tx.CardanoAPI             (toCardanoAddressInEra)
import           Ledger.Value                     (CurrencySymbol(..))
import           PlutusTx                         (Data (..), ToData(..), builtinDataToData)
import           PlutusTx.Builtins                (serialiseData)
import           PlutusTx.Prelude
import           Prelude                          (IO, String, Show (..), writeFile, print, unzip)
import qualified Prelude                          as Haskell
import           System.Random                    (randomIO)
import           Test.QuickCheck                  (quickCheck)
import           Text.Hex                         (decodeHex, encodeHex)

import           ENCOINS.Bulletproofs             (BulletproofSetup (..), Secret (..), Randomness (..),
                                                    Input (..), Proof(..), bulletproof, parseBulletproofParams)
import           ENCOINS.BaseTypes                (MintingPolarity(..), groupExp, groupGenerator, fromGroupElement)
import           ENCOINS.Core.OnChain             (encoinsPolicy, encoinsSymbol, beaconCurrencySymbol, stakingValidatorAddress)
import           ENCOINS.Core.V1.OnChain          (hashRedeemer)
import           ENCOINS.Crypto.Field             (Field(..))
import           PlutusAppsExtra.Utils.Address    (bech32ToAddress)
import           PlutusTx.Extra.ByteString        (toBytes)

-- A helper function to convert Plutus data to JSON
mkSchema :: Data -> String
mkSchema (I i) = "{\"int\": " ++ show i ++ "}"
mkSchema (B b) = "{\"bytes\": " ++ (show . encodeHex $ b) ++ "}"
mkSchema (List dats) = "{\"list\": [" ++ lst ++ "]}"
    where lst = intercalate ", " (map mkSchema dats)
mkSchema (Map entries) = "{\"map\": [" ++ lst ++ "]}"
    where
        (ks, vs) = unzip entries
        mkEntry k v = "{\"k\": " ++ mkSchema k ++ ", \"v\":" ++ mkSchema v ++ "}"
        lst = intercalate ", " (zipWith mkEntry ks vs)
mkSchema (Constr n dats) = "{ \"constructor\": " ++ show n ++ ", \"fields\": [" ++ lst ++ "]}"
    where lst = intercalate ", " (map mkSchema dats)

verifierPKH ::BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "BA1F8132201504C494C52CE3CC9365419D3446BD5A4DCDE19396AAC68070977D"

main :: IO ()
main = do
    let beaconSymb = beaconCurrencySymbol $
            TxOutRef (TxId $ toBuiltin $ fromJust $ decodeHex "171299765a0f14893a6880272e53689d8ceea8abe74168f494b29d0b6f751a9e") 2
        encoinsPar = (beaconSymb, verifierPKH)
        encoinsSymb = encoinsSymbol encoinsPar
        stakingAddr = serialiseAddress $ fromRight (error ()) $ toCardanoAddressInEra (Testnet $ NetworkMagic 2) $  stakingValidatorAddress encoinsSymb

    -- Writing a new bulletproof setup to JSON
    bulletproofSetup <- randomIO :: IO BulletproofSetup
    writeFileJSON "result/bulletproof_setup.json" bulletproofSetup
    -- Writing current currency symbol to JSON
    writeFileJSON "result/encoinsPolicyId.json" $ toJSON encoinsSymb
    -- Writing current staking address to JSON
    writeFileJSON "result/stakingAddr.json" $ toJSON stakingAddr

    print "Done!"