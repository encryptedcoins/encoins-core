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

import           ENCOINS.Bulletproofs             (Secret (..), Randomness (..), Input (..), Proof(..), bulletproof, parseBulletproofParams)
import           ENCOINS.BaseTypes                (MintingPolarity(..), groupExp, groupGenerator, fromGroupElement)
import           ENCOINS.Core.OnChain             (encoinsPolicy, bulletproofSetup, encoinsSymbol, beaconCurrencySymbol, stakingValidatorAddress)
import           ENCOINS.Core.V1.OffChain         (verifierPKH)
import           ENCOINS.Core.V1.OnChain          (hashRedeemer)
import           ENCOINS.Crypto.Field             (Field(..))
import           PlutusAppsExtra.Utils.Address    (bech32ToAddress)
import           PlutusTx.Extra.ByteString        (toBytes)

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
    let beaconSymb = CurrencySymbol $ toBuiltin $ fromJust $ decodeHex "4cd1187e477d56e419c354f1e4c7997a736dfc5e095a2511aba0f75d"
        encoinsPar = (beaconSymb, verifierPKH)
        encoinsSymb = encoinsSymbol encoinsPar
        stakingAddr = serialiseAddress $ fromRight (error ()) $ toCardanoAddressInEra (Testnet $ NetworkMagic 2) $  stakingValidatorAddress encoinsSymb

    print $ encoinsSymb
    print $ stakingValidatorAddress encoinsSymb

    -- Writing current bulletproof setup to JSON
    writeFileJSON "testnet/setup.json" bulletproofSetup
    -- Writing current minting script to JSON
    writeFileJSON "testnet/encoinsScript.json" $ toJSON $ encoinsPolicy encoinsPar

    -- Printing a test Redeemer in different formats
    let verifierPRV = fromJust $ decodeHex "1DA4194798C1D3AA8B7E5E39EDA1F130D9123ACCC8CA31A82E033A6D007DA7EC"
        beaconRef = TxOutRef (TxId $ toBuiltin $ fromJust $ decodeHex "3ee12f1e8b72eca75a41a582cc788def0dae622a51db65aea6a9d2798843d80c") 3
        addr = stakingValidatorAddress $ encoinsSymbol (beaconCurrencySymbol beaconRef, verifierPKH)
        bp = parseBulletproofParams $ toBytes addr
        s1 = Secret (F 78623591232) (F 3)
        s2 = Secret (F 21879124) (F 5)
        rs = Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)
        (v, inputs, proof) = bulletproof bulletproofSetup bp [s1, s2] [Mint, Mint] rs
        inputs' = map (\(Input g p) -> (fromGroupElement g, p)) inputs
        red = (addr, (v, inputs'), proof, "")
        msg = hashRedeemer red
        sig = toBuiltin $ fromJust $ decodeHex "4A9B88E284300489ED71F80D169D16317322E390EFFB7DB14ED707F8F696E3F4BC23759E7FC5779655116FBEB336D6C2EFD44ADACF9FDB5B69BFD8DF7D41A201"
        red' = (addr, (v, inputs'), proof, sig)
    writeFile "testnet/Schema" $ mkSchema $ builtinDataToData $ toBuiltinData red
    print $ encodeHex $ fromBuiltin $ serialiseData $ toBuiltinData red
    writeFileJSON "testnet/redeemer.json" red'
    
    print "Done!"