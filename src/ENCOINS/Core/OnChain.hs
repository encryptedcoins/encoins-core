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
import           Ledger.Value                         (geq, noAdaValue)
import           Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes (..), TypedValidator,
                                                        mkTypedValidator, mkUntypedValidator, mkUntypedMintingPolicy)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts            (findOwnInput)
import           PlutusTx                             (compile, applyCode, liftCode)
import           PlutusTx.AssocMap                    (lookup)
import           PlutusTx.Prelude

import           Constraints.OnChain                  (tokensMinted, filterUtxoSpent, filterUtxoProduced)
import           ENCOINS.Core.Bulletproofs
import           ENCOINS.Core.BaseTypes
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

----------------------------------- ENCOINS Minting Policy ---------------------------------------

bulletproofSetup :: BulletproofSetup
bulletproofSetup = BulletproofSetup groupGenerator (groupExp groupGenerator (toFieldElement 2))
  (map (groupExp groupGenerator . toFieldElement) [3..(bulletproofN * bulletproofM + 2)])
  (map (groupExp groupGenerator . toFieldElement) [(bulletproofN * bulletproofM + 3)..(2 * (bulletproofN * bulletproofM) + 2)])

-- Beacon currency symbol
type EncoinsParams = CurrencySymbol

type TxParams = (Integer, Address, PubKeyHash, (POSIXTime, POSIXTime))

type EncoinsRedeemer = (TxParams, [(BuiltinByteString, MintingPolarity)], Proof)

{-# INLINABLE encoinName #-}
encoinName :: BuiltinByteString -> TokenName
encoinName = TokenName . sha2_256

-- TODO: remove on-chain sorting
encoinsPolicyCheck :: EncoinsParams -> EncoinsRedeemer -> ScriptContext -> Bool
encoinsPolicyCheck _ ((v, addr, pkh, (_, _)), inputs, _)
    ctx@ScriptContext{scriptContextTxInfo=info} =
      cond0
      -- && cond1
      && cond2
      -- && cond3
      -- && cond4
      && cond5
  where
      -- beacon = token (AssetClass (beaconSymb, beaconTokenName))
      -- bp     = toBytes addr `appendByteString` toBytes pkh `appendByteString` toBytes (getPOSIXTime tFrom) `appendByteString` toBytes (getPOSIXTime tTo)
      val    = lovelaceValueOf (abs v * 1_000_000)

      cond0 = tokensMinted ctx $ fromList $ sort $ map (\(bs, p) -> (encoinName bs, polarityToInteger p)) inputs
      -- cond1 = verify bulletproofSetup (toGroupElement bp) v inputs proof
      cond2 = sum (map txOutValue $ filterUtxoProduced info (\o -> txOutAddress o == addr)) `geq` val
      -- cond3 = utxoReferenced info (\o -> txOutAddress o == addr && txOutValue o `geq` beacon) || (v < 0)
      -- cond4 = validatedInInterval info tFrom tTo
      cond5 = pkh `elem` txInfoSignatories info

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
    val  = lovelaceValueOf $ (* 1_000_000) $ fromMaybe 0 $ do
      red <- lookup purp $ txInfoRedeemers info
      ((v, _, _, _), _, _) <- fromBuiltinData $ getRedeemer red :: Maybe EncoinsRedeemer
      Just v

    cond0 = vIn == (vOut + val)

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
