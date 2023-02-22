{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.OffChain where

import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                               hiding ((<$>))

import           ENCOINS.Core.V1.OffChain                       as V1
import           ENCOINS.Core.V1.OnChain                        as V1
import           PlutusAppsExtra.Types.Tx                       (TransactionBuilder)

---------------------------- Stake Owner Token Minting Policy --------------------------------------

stakeOwnerMintTx :: TxOutRef -> TransactionBuilder ()
stakeOwnerMintTx = V1.stakeOwnerMintTx

stakeOwnerTx :: TxOutRef -> TransactionBuilder ()
stakeOwnerTx = V1.stakeOwnerTx

------------------------------------- Beacon Minting Policy --------------------------------------

beaconMintTx :: TxOutRef -> TransactionBuilder ()
beaconMintTx = V1.beaconMintTx

beaconSendTx :: TxOutRef -> BuiltinByteString -> TxOutRef -> TransactionBuilder ()
beaconSendTx = V1.beaconSendTx

beaconTx :: TxOutRef -> BuiltinByteString -> TxOutRef -> TransactionBuilder ()
beaconTx = V1.beaconTx

----------------------------------- ENCOINS Minting Policy ---------------------------------------

encoinsBurnTx :: V1.EncoinsParams -> [BuiltinByteString] -> TransactionBuilder Value
encoinsBurnTx = V1.encoinsBurnTx

encoinsTx :: (Address, Address) -> V1.EncoinsParams -> V1.EncoinsStakeParams -> V1.EncoinsRedeemer -> EncoinsMode -> TransactionBuilder ()
encoinsTx = V1.encoinsTx

postEncoinsPolicyTx :: V1.EncoinsParams -> Integer -> TransactionBuilder ()
postEncoinsPolicyTx = V1.postEncoinsPolicyTx

------------------------------------- ENCOINS Ledger Validator --------------------------------------

-- Spend utxo greater than the given value from the ENCOINS Ledger script.
stakingSpendTx' :: V1.EncoinsSpendParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx' = V1.ledgerSpendTx'

-- Spend utxo greater than the given value from the ENCOINS Ledger script. Fails if the utxo is not found.
stakingSpendTx :: V1.EncoinsSpendParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx = V1.ledgerSpendTx

-- Combines several utxos into one.
stakingCombineTx :: V1.EncoinsSpendParams -> Value -> Integer -> TransactionBuilder ()
stakingCombineTx = V1.ledgerCombineTx

-- Modify the value locked in staking by the given value
stakingModifyTx :: V1.EncoinsSpendParams -> Value -> TransactionBuilder ()
stakingModifyTx = V1.ledgerModifyTx

postStakingValidatorTx :: V1.EncoinsLedgerParams -> Integer -> TransactionBuilder ()
postStakingValidatorTx = V1.postStakingValidatorTx