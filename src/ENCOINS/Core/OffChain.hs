{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.OffChain where

import           Ledger.Value                                   (AssetClass (..))
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.Core.V1.OffChain                       as V1
import           ENCOINS.Core.V1.OnChain                        as V1
import           Types.Tx                                       (TransactionBuilder)

------------------------------------- Beacon Minting Policy --------------------------------------

beaconCurrencySymbol :: TxOutRef -> CurrencySymbol
beaconCurrencySymbol = V1.beaconCurrencySymbol

beaconAssetClass :: TxOutRef -> AssetClass
beaconAssetClass = V1.beaconAssetClass

beaconToken :: TxOutRef -> Value
beaconToken = V1.beaconToken

beaconMintTx :: TxOutRef -> TransactionBuilder ()
beaconMintTx = V1.beaconMintTx

beaconSendTx :: TxOutRef -> TransactionBuilder ()
beaconSendTx = V1.beaconSendTx

----------------------------------- ENCOINS Minting Policy ---------------------------------------

encoinsSymbol :: V1.EncoinsParams -> CurrencySymbol
encoinsSymbol = V1.encoinsSymbol

encoinsAssetClass :: V1.EncoinsParams -> BuiltinByteString -> AssetClass
encoinsAssetClass = V1.encoinsAssetClass

encoin :: V1.EncoinsParams -> BuiltinByteString -> Value
encoin = V1.encoin

encoinsBurnTx :: V1.EncoinsParams -> BuiltinByteString -> TransactionBuilder ()
encoinsBurnTx = V1.encoinsBurnTx

encoinsTx :: V1.EncoinsParams -> V1.EncoinsRedeemer -> TransactionBuilder ()
encoinsTx = V1.encoinsTx

------------------------------------- ADA Staking Validator --------------------------------------

stakingValidator :: V1.StakingParams -> Validator
stakingValidator = V1.stakingValidator

stakingValidatorHash :: V1.StakingParams -> ValidatorHash
stakingValidatorHash = V1.stakingValidatorHash

stakingValidatorAddress :: V1.StakingParams -> Address
stakingValidatorAddress = V1.stakingValidatorAddress

-- Spend utxo greater than the given value from the Staking script.
stakingSpendTx' :: V1.StakingParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx' = V1.stakingSpendTx'

-- Spend utxo greater than the given value from the Staking script. Fails if the utxo is not found.
stakingSpendTx :: V1.StakingParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx = V1.stakingSpendTx

-- Combines several utxos into one.
stakingCombineTx :: V1.StakingParams -> Value -> Integer -> TransactionBuilder ()
stakingCombineTx = V1.stakingCombineTx

-- Modify the value locked in staking by the given value
stakingModifyTx :: V1.StakingParams -> Value -> TransactionBuilder ()
stakingModifyTx = V1.stakingModifyTx

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

ledgerValidator :: Validator
ledgerValidator = V1.ledgerValidator

ledgerValidatorHash :: ValidatorHash
ledgerValidatorHash = V1.ledgerValidatorHash

ledgerValidatorAddress :: Address
ledgerValidatorAddress = V1.ledgerValidatorAddress

ledgerTx :: V1.EncoinsParams -> [BuiltinByteString] -> TransactionBuilder ()
ledgerTx = V1.ledgerTx
        