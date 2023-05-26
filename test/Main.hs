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
import           ENCOINS.Core.OnChain             (encoinsPolicy, encoinsSymbol, beaconCurrencySymbol, ledgerValidatorAddress, toEncoinsPolicyParams)
import           ENCOINS.Crypto.Field             (Field(..))
import           PlutusAppsExtra.Utils.Address    (bech32ToAddress, addressToBech32)
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
    
main :: IO ()
main = do
    let encoinsPar     = (
                TxOutRef (TxId $ toBuiltin $ fromJust $ decodeHex "274f23b63b921eabd62c2701eb2ca3fddf69644f1bb34d1b4ba6eee7290e8681") 4,
                TxOutRef (TxId $ toBuiltin $ fromJust $ decodeHex "5e2012679f0a7ad74ac9c50de1861175653675db68807ec6d608cb949399ec20") 6,
                toBuiltin $ fromJust $ decodeHex "BA1F8132201504C494C52CE3CC9365419D3446BD5A4DCDE19396AAC68070977D"
            )
        encoinsSymb    = encoinsSymbol encoinsPar
        ledgerAddr     = ledgerValidatorAddress encoinsPar

    -- Writing a new bulletproof setup to JSON
    bulletproofSetup <- randomIO :: IO BulletproofSetup
    writeFileJSON "result/bulletproof_setup.json" bulletproofSetup
    -- Printing ENCOINS minting policy parameters
    writeFileJSON "result/encoinsPolicyParameters.json" $ toEncoinsPolicyParams encoinsPar
    -- Writing current currency symbol to JSON
    writeFileJSON "result/encoinsPolicyId.json" $ toJSON encoinsSymb
    -- Writing current staking address to JSON
    writeFileJSON "result/ledgerAddr.json" $ toJSON ledgerAddr

    print "Done!"