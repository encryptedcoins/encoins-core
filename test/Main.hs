{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Cardano.Api                   (NetworkId (..), NetworkMagic (..), SerialiseAddress (..), writeFileJSON)
import           Data.Aeson                    (ToJSON (..))
import           Data.Either                   (fromRight)
import           Data.List                     (intercalate)
import           Data.Maybe                    (fromJust)
import           Data.Text                     (pack)
import           Ledger                        (TxId (..), TxOutRef (..))
import           Ledger.Tx.CardanoAPI          (toCardanoAddressInEra)
import           PlutusTx                      (Data (..), ToData (..), builtinDataToData)
import           PlutusTx.Builtins             (serialiseData)
import           PlutusTx.Prelude
import           Prelude                       (IO, Show (..), String, print, unzip, writeFile)
import qualified Prelude                       as Haskell
import           System.Random                 (randomIO)
import           Test.QuickCheck               (quickCheck)
import           Text.Hex                      (decodeHex, encodeHex)

import           ENCOINS.BaseTypes             (MintingPolarity (..), fromGroupElement, groupExp, groupGenerator)
import           ENCOINS.Bulletproofs          (BulletproofSetup (..), Input (..), Proof (..), Randomness (..), Secret (..),
                                                bulletproof, parseBulletproofParams)
import           ENCOINS.Core.OnChain          (beaconCurrencySymbol, encoin, encoinsPolicy, encoinsSymbol,
                                                ledgerValidatorAddress, toEncoinsPolicyParams)
import           ENCOINS.Crypto.Field          (Field (..))
import           PlutusAppsExtra.Utils.Address (addressToBech32, bech32ToAddress)
import           PlutusTx.Extra.ByteString     (toBytes)
import           Script                        (scriptSpec)
import           Test.Hspec                    (hspec)
import           Tx                            (txSpec)

main :: IO ()
main = do
    writeEncoinsSetup
    hspec $ do
        scriptSpec
        txSpec

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

writeEncoinsSetup :: IO ()
writeEncoinsSetup = do
    let encoinsPar     = (
                TxOutRef (TxId $ toBuiltin $ fromJust $ decodeHex "8b6077ddc93fbe3bc20360ba186b168639a91aa0983a1cc4ff60213f62a23670") 2,
                TxOutRef (TxId $ toBuiltin $ fromJust $ decodeHex "9da0957fe1e2c3fa1a898a6453d0c43f6158f1d75c235669c64deb5f7881a8be") 1,
                toBuiltin $ fromJust $ decodeHex "7F04730FC0F75A7D20BEB8CD152B2B8571591282EC8E5A3FC266C52049A3A5C6"
            )
        encoinsSymb    = encoinsSymbol encoinsPar
        ledgerAddr     = ledgerValidatorAddress encoinsPar

    -- Writing a new bulletproof setup to JSON
    bulletproofSetup <- randomIO :: IO BulletproofSetup
    writeFileJSON "result/bulletproof_setup.json" bulletproofSetup
    -- Writing ENCOINS minting policy parameters to JSON
    writeFileJSON "result/encoinsPolicyParameters.json" $ toEncoinsPolicyParams encoinsPar
    -- Writing ENCOINS currency symbol to JSON
    writeFileJSON "result/encoinsPolicyId.json" $ toJSON encoinsSymb
    -- Writing ENCOINS minting policy to JSON
    writeFileJSON "result/encoinsPolicy.json" $ toJSON $ encoinsPolicy encoinsPar
    -- Writing current ENCOINS Ledger address to JSON
    writeFileJSON "result/ledgerAddr.json" $ toJSON ledgerAddr

    print "Done!"