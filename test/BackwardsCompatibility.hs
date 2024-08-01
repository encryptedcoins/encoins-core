{-# LANGUAGE OverloadedStrings #-}

module BackwardsCompatibility where

import           Control.Lens             (over)
import           ENCOINS.Core.OnChain     (Aiken (..), EncoinsPolicyParams, EncoinsProtocolParams (..), beaconAssetClass,
                                           beaconCurrencySymbol, beaconPolicyV, beaconToken, encoinsPolicy, encoinsPolicyV,
                                           encoinsStakeValidatorHash, ledgerValidatorHash, stakeOwnerPolicyV, stakeOwnerToken,
                                           toEncoinsPolicyParams)
import           Ledger                   (MintingPolicyHash, StakeValidatorHash, TxOutRef (TxOutRef), ValidatorHash, mintingPolicyHash)
import           Ledger.Scripts           (scriptCurrencySymbol)
import           PlutusCore               (NamedDeBruijn)
import qualified PlutusCore               as PLC
import           PlutusCore.Builtin.Debug (plcVersion100)
import           PlutusCore.Default       (DefaultFun, DefaultUni (..))
import           PlutusLedgerApi.Common   (SerialisedScript, serialiseUPLC)
import qualified PlutusLedgerApi.V1       as PV1
import qualified PlutusLedgerApi.V1.Value as PV1
import           PlutusTx                 (Data, getPlc, getPlcNoAnn, liftCode, toData)
import           Test.Hspec               (Spec, describe, it, shouldBe)
import qualified UntypedPlutusCore        as UPLC

backwardsCompatibilitySpec :: Spec
backwardsCompatibilitySpec = describe "Backwards compatibility" $ do
    it "encoins policy"          $ encoinsPolicyHashExpected         `shouldBe` "922efbdd1eded224ec0c3db26344f3a616b58116d889b685ecf97964"
    it "ledger validator"        $ ledgerValidatorHashExpected       `shouldBe` "ffe495b7b3c4bea825284bf80981ebf98bdc8a3f6de5f14355c42eb8"
    it "stake owner policy"      $ stakeOwnerPolicyHashExpected      `shouldBe` "d1f723925a767761cfe8e54d55f7d435e61bb0818ec3d67b229f3ca2" -- ok
    it "beacon policy"           $ beaconPolicyExpected              `shouldBe` "4af1e0dacde9822f91d94af6eac75637f8ddf32872b0a22b9ca38fbb" -- ok
    it "encoins stake validator" $ encoinsStakeValidatorHashExpected `shouldBe` "d977489e351b68e5b0b7440b39d4c176d3a7504d2945a7d35f418ba1"

encoinsParamExaple :: EncoinsProtocolParams
encoinsParamExaple =
    ( TxOutRef "0f27a9384e175a98784932813bc087fbfe10afbb87d4c2bae7c3da2357a0effc" 5
    , TxOutRef "f25655007d89a1459fc0b2933e89888a7955dc8560e0440cb371a5c0a45d5967" 2
    , "7F04730FC0F75A7D20BEB8CD152B2B8571591282EC8E5A3FC266C52049A3A5C6"
    , "0bd016f8ba5857d2e2026da550e4b724a3e1e8d5598cbfab19ce756c"
    )

encoinsPolicyHashExpected :: MintingPolicyHash
encoinsPolicyHashExpected = mintingPolicyHash $ encoinsPolicyV encoinsParamExaple

ledgerValidatorHashExpected :: ValidatorHash
ledgerValidatorHashExpected = ledgerValidatorHash encoinsParamExaple

stakeOwnerPolicyHashExpected :: MintingPolicyHash
stakeOwnerPolicyHashExpected = mintingPolicyHash $ stakeOwnerPolicyV encoinsParamExaple

beaconPolicyExpected :: MintingPolicyHash
beaconPolicyExpected = mintingPolicyHash $ beaconPolicyV encoinsParamExaple

beaconCurrencySymbolExpected :: PV1.CurrencySymbol
beaconCurrencySymbolExpected = beaconCurrencySymbol encoinsParamExaple

encoinsStakeValidatorHashExpected :: StakeValidatorHash
encoinsStakeValidatorHashExpected = encoinsStakeValidatorHash encoinsParamExaple

--------------------------------------------------------

encoinsPolicyParamExample :: EncoinsPolicyParams
encoinsPolicyParamExample = toEncoinsPolicyParams encoinsParamExaple

beaconTokenExpected :: PV1.Value
beaconTokenExpected = beaconToken encoinsParamExaple

beaconAssetClassExpected :: PV1.AssetClass
beaconAssetClassExpected = beaconAssetClass encoinsParamExaple

toNameless ::
      UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () ->
      UPLC.Program UPLC.DeBruijn DefaultUni DefaultFun ()
toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn
