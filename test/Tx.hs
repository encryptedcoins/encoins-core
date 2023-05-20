{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Tx where

import           Cardano.Node.Emulator            (Params (..))
import           Control.Monad                    (replicateM, when)
import           Control.Monad.State              (modify)
import           Data.Aeson                       (FromJSON (..), eitherDecodeFileStrict, genericParseJSON)
import           Data.Aeson.Casing                (aesonPrefix, snakeCase)
import           Data.Bifunctor                   (Bifunctor (bimap))
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromJust)
import           ENCOINS.BaseTypes                (MintingPolarity (..))
import           ENCOINS.Bulletproofs             (Secret (..), bulletproof, parseBulletproofParams)
import           ENCOINS.Bulletproofs.Prove       (fromSecret)
import           ENCOINS.Core.OffChain            (encoinsTx)
import           ENCOINS.Core.V1.OffChain         (EncoinsMode (..))
import           ENCOINS.Core.V1.OnChain          (EncoinsProtocolParams, beaconToken, encoinsStakeValidatorHash, encoinsSymbol,
                                                   hashRedeemer, ledgerValidatorAddress, stakeOwnerToken)
import           ENCOINS.Crypto.Field             (toFieldElement)
import           GHC.Generics                     (Generic)
import           Gen                              (BurnRequest, EncoinsRequest (..), MintRequest, MixedRequest)
import           Ledger                           (Address (..), CurrencySymbol, DecoratedTxOut (..), NetworkId,
                                                   StakeValidatorHash (..), TxOutRef (..), ValidatorHash (..), Value)
import           Ledger.Value                     (TokenName (..), Value (..))
import           Plutus.V2.Ledger.Api             (Credential (..), StakingCredential (..))
import           PlutusAppsExtra.Test.Utils       (TxTestM, buildTx, genCollateral, genPubKeyAddress,
                                                   genPubKeyAddressWithStakingHash, genTxOutRef, getProtocolParams, runTxTest,
                                                   withAdaUtxo, withValueUtxo)
import           PlutusAppsExtra.Utils.Address    (bech32ToAddress)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Crypto     (sign)
import           PlutusAppsExtra.Utils.Datum      (inlinedUnitInTxOut)
import qualified PlutusTx.AssocMap                as PAM
import           PlutusTx.Builtins                (BuiltinByteString, sha2_256, toBuiltin)
import           PlutusTx.Extra.ByteString        (toBytes)
import           System.Random                    (randomIO)
import           Test.Hspec                       (Expectation, context, describe, hspec, it, parallel)
import           Test.QuickCheck                  (property, withMaxSuccess)
import           Text.Hex                         (decodeHex)

type HasTxTestEnv =
    ( ?pParams            :: Params
    , ?mode               :: EncoinsMode
    , ?ledgerAddr         :: Address
    , ?verifierPrvKey     :: BuiltinByteString
    , ?encoinsCS          :: CurrencySymbol
    , ?encoinsParams      :: EncoinsProtocolParams
    , ?encoinsParamsUtxos :: MapUTXO
    )

data TestConfig = TestConfig
    { tcProtocolParamsFile :: FilePath
    , tcVerifierPkhFile    :: FilePath
    , tcVerifierPrvKeyFile :: FilePath
    , tcNetworkId          :: NetworkId
    } deriving (Show, Generic)

instance FromJSON TestConfig where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

main :: IO ()
main = do
    TestConfig{..} <- either error id <$> eitherDecodeFileStrict "test/testConfig.json"
    verifierPKH    <- either error id <$> eitherDecodeFileStrict tcVerifierPkhFile
    verifierPrvKey <- either error id <$> eitherDecodeFileStrict tcVerifierPrvKeyFile
    p              <- getProtocolParams tcProtocolParamsFile tcNetworkId

    let refBeacon     = TxOutRef "961c9f01189852e298e46c3e48bb63616a7ddaa05210fd13af06f95f1db99fc2" 0
        refStakeOwner = TxOutRef "53cafd08c8f309d0fbdff986b65dfbe9008f1d6658eed48d736d89c4a2e522a2" 0
        encoinsParams = (refStakeOwner, refBeacon, verifierPKH)
        ledgerAddr    = ledgerValidatorAddress encoinsParams
        ledgerVh = let Address (ScriptCredential vh) _ = ledgerAddr 
                   in vh
        ledgerSc = let StakeValidatorHash vh = encoinsStakeValidatorHash encoinsParams 
                   in Just $ StakingHash $ ScriptCredential $ ValidatorHash vh

        stakeOwnerOut = ScriptDecoratedTxOut ledgerVh ledgerSc (stakeOwnerToken encoinsParams) inlinedUnitInTxOut Nothing Nothing
        beaconOut = ScriptDecoratedTxOut ledgerVh ledgerSc (beaconToken encoinsParams) inlinedUnitInTxOut Nothing Nothing
    
    let ?pParams            = p
        ?ledgerAddr         = ledgerAddr
        ?verifierPrvKey     = verifierPrvKey
        ?encoinsParams      = encoinsParams
        ?encoinsCS          = encoinsSymbol encoinsParams
        ?encoinsParamsUtxos = Map.fromList [(refStakeOwner, stakeOwnerOut), (refBeacon, beaconOut)]
   
    hspec $ parallel $ describe "encoinsTx" $

        context "fixed utxos" $ do

            context "wallet mode" $ do
                let ?mode = WalletMode

                it "mint" $ withMaxSuccess 10 $ property $ encoinsTxTest @MintRequest $ do
                    withAdaUtxo (?reqSum + 2) ?changeAddr

                it "burn" $ withMaxSuccess 10 $ property $ encoinsTxTest @BurnRequest $ do
                    withAdaUtxo (negate ?reqSum + 2) ?ledgerAddr
                    withValueUtxo ?burnVal ?changeAddr

                it "mix" $ withMaxSuccess 10 $ property $ encoinsTxTest @MixedRequest $ do
                    withAdaUtxo (abs ?reqSum + 2) $ if ?reqSum > 0 then ?changeAddr else ?ledgerAddr
                    when (?reqSum < 2 && ?reqSum > 0) $ withAdaUtxo 2 ?ledgerAddr
                    withValueUtxo ?burnVal ?changeAddr

            context "ledger mode" $ do
                let ?mode = LedgerMode

                it "mint" $ withMaxSuccess 10 $ property $ encoinsTxTest @MintRequest $ do
                    withAdaUtxo (?reqSum + 2) ?changeAddr

                it "burn" $ withMaxSuccess 10 $ property $ encoinsTxTest @BurnRequest $ do
                    withAdaUtxo (negate ?reqSum + 2) ?ledgerAddr
                    withValueUtxo ?burnVal ?ledgerAddr

                it "mix" $  withMaxSuccess 10 $ property $ encoinsTxTest @MixedRequest $ do
                    withAdaUtxo (abs ?reqSum + 3) $ if ?reqSum > 0 then ?changeAddr else ?ledgerAddr
                    when (?reqSum < 2 && ?reqSum > 0) $ withAdaUtxo 2 ?ledgerAddr
                    withValueUtxo ?burnVal ?ledgerAddr

type HasEncoinsTxTestEnv =
    ( ?changeAddr :: Address
    , ?req        :: [Integer]
    , ?reqSum     :: Integer
    , ?burnVal    :: Value
    )

encoinsTxTest :: HasTxTestEnv => EncoinsRequest req => (HasEncoinsTxTestEnv => TxTestM ()) -> req -> Expectation
encoinsTxTest addUtxos (extractRequest -> req) = do
    addrRelay        <- genPubKeyAddress
    addrTreasury     <- genPubKeyAddress
    -- changeAddr without staking creds lead to incomprehensible error about redeemer ptr
    changeAddr       <- genPubKeyAddressWithStakingHash
    collateral       <- genCollateral
    gammas           <- replicateM (length req) randomIO
    randomness       <- randomIO
    bulletproofSetup <- randomIO

    let ps = map (\i -> if i < 0 then Burn else Mint) req
        secrets = zipWith (\g i -> Secret g $ toFieldElement i) gammas req
        bss = map (snd . fromSecret bulletproofSetup) secrets
        inputs = zip bss ps
        par = (?ledgerAddr, changeAddr)
        bp  = parseBulletproofParams $ sha2_256 $ toBytes par
        (_, _, proof) = bulletproof bulletproofSetup bp secrets ps randomness
        input = (sum req, inputs)
        redOnChain = (par, input, sha2_256 $ toBytes proof, "")
        redOnChain' = (par, input, sha2_256 $ toBytes proof, sign ?verifierPrvKey $ hashRedeemer redOnChain)

    let ?changeAddr = changeAddr
        ?req        = req
        ?reqSum     = sum req
        ?burnVal    = Value $ PAM.singleton ?encoinsCS $ PAM.fromList $ bimap TokenName (const 1) <$> filter ((== Burn) . snd) inputs
    
    runTxTest $ do
        modify (<> ?encoinsParamsUtxos)
        addUtxos
        buildTx ?pParams collateral ?ledgerAddr [encoinsTx (addrRelay, addrTreasury) ?encoinsParams redOnChain' ?mode]