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
import           Ledger.Tx.CardanoAPI             (toCardanoAddressInEra)
import           Ledger.Value                     (CurrencySymbol(..))
import           PlutusTx                         (Data (..), ToData(..), builtinDataToData)
import           PlutusTx.Builtins                (serialiseData)
import           PlutusTx.Prelude
import           Prelude                          (IO, print, unzip, String, Show (..), writeFile)
import qualified Prelude                          as Haskell
import           Test.QuickCheck                  (quickCheck)
import           Text.Hex                         (decodeHex, encodeHex)

import           ENCOINS.Bulletproofs             (Secret (..), Randomness (..), Input (..), Proof(..), bulletproof)
import           ENCOINS.BaseTypes                (MintingPolarity(..), groupExp, groupGenerator, fromGroupElement)
import           ENCOINS.Core.OffChain            (encoinsSymbol, stakingValidatorAddress)
import           ENCOINS.Core.OnChain             (encoinsPolicy, bulletproofSetup)
import           ENCOINS.Core.V1.OffChain         (verifierPKH)
import           ENCOINS.Core.V1.OnChain          (hashRedeemer)
import           ENCOINS.Crypto.Field             (Field(..))
import           Utils.Address                    (bech32ToAddress)

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

    -- Writing current bulletproof setup to JSON
    writeFileJSON "testnet/setup.json" bulletproofSetup
    -- Writing current minting script to JSON
    writeFileJSON "testnet/encoinsScript.json" $ toJSON $ encoinsPolicy encoinsPar

    -- Printing a test Redeemer in different formats
    let verifierPRV = fromJust $ decodeHex "1DA4194798C1D3AA8B7E5E39EDA1F130D9123ACCC8CA31A82E033A6D007DA7EC"
        g = groupGenerator
        bp = groupExp g (F 176)
        s1 = Secret (F 78623591232) (F 3)
        s2 = Secret (F 21879124) (F 5)
        rs = Randomness (F 3417) (map F [1..20]) (map F [21..40]) (F 8532) (F 16512) (F 1235)
        (v, inputs, proof) = bulletproof bulletproofSetup bp [s1, s2] [Mint, Mint] rs
        inputs' = map (\(Input g p) -> (fromGroupElement g, p)) inputs
        red = (fromJust $ bech32ToAddress "addr_test1qr297zyzq4kaafaypq599x66vxznytjyfs39gx9endwyksf9x5zdat37w6pt3lzvqkumrpdkyjf8faxek2xkjd59n0csw4dvtx",
            (v, inputs'), proof, sig)
        msg = hashRedeemer red
        sig = toBuiltin $ fromJust $ decodeHex "AF26CA8095F1CD9996237878DDF2380FAAF64D045D1C7B1EBB6F1C25F450FA88D4AEBED827878FD8ACEF7C7501C4E2C1884F5FFB8C3ADED7CD607A7425705D0A"
    writeFile "testnet/Schema" $ mkSchema $ builtinDataToData $ toBuiltinData red
    print $ encodeHex $ fromBuiltin $ serialiseData $ toBuiltinData red
    
    print "Done!"