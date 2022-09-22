{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}


module ENCOINS.Core.OnChain where

import           Ledger.Ada                           (lovelaceValueOf)
import           Ledger.Tokens                        (token)
import           Ledger.Value                         (AssetClass (..), geq, noAdaValue)
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), TypedValidator,
                                                        mkTypedValidator, mkUntypedValidator, mkUntypedMintingPolicy)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            (findOwnInput)
import           PlutusTx                             (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                    (lookup)
import           PlutusTx.Prelude

import           ENCOINS.Core.Bulletproofs            (Input, Proof, verify, polarityToInteger)
import           ENCOINS.Core.Types                   (GroupElement, fromFieldElement)
import           Scripts.Constraints                  (utxoReferenced, tokensMinted, filterUtxoSpent, filterUtxoProduced, utxoSpent)
import           Scripts.OneShotCurrency              (OneShotCurrencyParams, mkCurrency, oneShotCurrencyPolicy)


------------------------------------- Beacon Minting Policy --------------------------------------

{-# INLINABLE beaconTokenName #-}
beaconTokenName :: TokenName
beaconTokenName = TokenName emptyByteString

{-# INLINABLE beaconParams #-}
beaconParams :: TxOutRef -> OneShotCurrencyParams
beaconParams ref = mkCurrency ref [(beaconTokenName, 1)]

beaconPolicy :: TxOutRef -> MintingPolicy
beaconPolicy = oneShotCurrencyPolicy . beaconParams

---------------------------------- ENCOINS Minting Policy --------------------------------------

-- Beacon currency symbol
type EncoinsParams = CurrencySymbol

type EncoinsRedeemer = (Address, Input, Proof)

{-# INLINABLE encoinName #-}
encoinName :: GroupElement -> TokenName
encoinName = TokenName

encoinsPolicyCheck :: EncoinsParams -> EncoinsRedeemer -> ScriptContext -> Bool
encoinsPolicyCheck beaconSymb (addr, input@(coins, v), proof)
    ctx@ScriptContext{scriptContextTxInfo=info} = cond0 && cond1 && cond2 && cond3
  where
      beacon = token (AssetClass (beaconSymb, beaconTokenName))
      val    = fromFieldElement v

      cond0 = tokensMinted ctx $ fromList $ map (\(g, p) -> (encoinName g, polarityToInteger p)) coins
      cond1 = verify input proof
      cond2 = utxoSpent info (\o -> txOutAddress o == addr) || -- we do not need to check that we withdraw the correct value here
        sum (map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == addr)) == lovelaceValueOf val
      cond3 = utxoReferenced info (\o -> txOutAddress o == addr && txOutValue o `geq` beacon)

encoinsPolicy :: EncoinsParams -> MintingPolicy
encoinsPolicy par = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . encoinsPolicyCheck ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode par

------------------------------------- ADA Staking Validator --------------------------------------

-- ENCOINS currency symbol
type StakingParams = CurrencySymbol

data StakingADA
instance ValidatorTypes StakingADA where
  type instance DatumType StakingADA = ()
  type instance RedeemerType StakingADA = ()

{-# INLINABLE stakingValidatorCheck #-}
stakingValidatorCheck :: StakingParams -> () -> () -> ScriptContext -> Bool
stakingValidatorCheck encoinsSymb _ _
    ctx@ScriptContext{scriptContextTxInfo=info} = cond0
  where
    purp = Minting encoinsSymb
    addr = txOutAddress $ txInInfoResolved $ fromMaybe (error ()) $ findOwnInput ctx -- this script's address
    vOut = sum $ map txOutValue $ filterUtxoSpent info (\o -> txOutAddress o == addr)
    vIn  = sum $ map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == addr)
    v    = lovelaceValueOf $ fromMaybe 0 $ do
      red <- lookup purp $ txInfoRedeemers info
      (_, (_, vFE), _) <- fromBuiltinData $ getRedeemer red :: Maybe EncoinsRedeemer
      Just $ fromFieldElement vFE

    cond0 = vIn == (vOut + v)

stakingTypedValidator :: StakingParams -> TypedValidator StakingADA
stakingTypedValidator par = mkTypedValidator @StakingADA
    ($$(PlutusTx.compile [|| stakingValidatorCheck ||])
    `PlutusTx.applyCode` PlutusTx.liftCode par)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @() @()

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

data Ledgering
instance ValidatorTypes Ledgering where
  type instance DatumType Ledgering = ()
  type instance RedeemerType Ledgering = ()

{-# INLINABLE ledgerValidatorCheck #-}
ledgerValidatorCheck :: () -> () -> ScriptContext -> Bool
ledgerValidatorCheck _ _
    ctx@ScriptContext{scriptContextTxInfo=info} = cond0
  where
    addr  = txOutAddress $ txInInfoResolved $ fromMaybe (error ()) $ findOwnInput ctx -- this script's address
    vOut  = noAdaValue $ sum $ map txOutValue $ filterUtxoSpent info (\o -> txOutAddress o == addr)
    vIn   = noAdaValue $ sum $ map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == addr)
    vMint = txInfoMint $ scriptContextTxInfo ctx

    cond0 = vIn == (vOut + vMint)

ledgerTypedValidator :: TypedValidator Ledgering
ledgerTypedValidator = mkTypedValidator @Ledgering
    $$(PlutusTx.compile [|| ledgerValidatorCheck ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @() @()
