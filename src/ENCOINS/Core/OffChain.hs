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

import           ENCOINS.Core.OnChain
import qualified ENCOINS.Core.V1.OffChain                       as V1
import           PlutusAppsExtra.Types.Tx                       (TransactionBuilder)

type EncoinsMode = V1.EncoinsMode

protocolFee :: Integer -> EncoinsMode -> Value
protocolFee = V1.protocolFee

---------------------------- Stake Owner Token Minting Policy --------------------------------------

stakeOwnerMintTx :: EncoinsProtocolParams -> TransactionBuilder ()
stakeOwnerMintTx = V1.stakeOwnerMintTx

stakeOwnerTx :: EncoinsProtocolParams -> TransactionBuilder ()
stakeOwnerTx = V1.stakeOwnerTx

------------------------------------- Beacon Minting Policy --------------------------------------

beaconMintTx :: EncoinsProtocolParams -> TransactionBuilder ()
beaconMintTx = V1.beaconMintTx

beaconSendTx :: EncoinsProtocolParams -> TransactionBuilder ()
beaconSendTx = V1.beaconSendTx

beaconTx :: EncoinsProtocolParams -> TransactionBuilder ()
beaconTx = V1.beaconTx

---------------------------------------- Posting Scripts ------------------------------------------

postEncoinsPolicyTx :: EncoinsProtocolParams -> Integer -> TransactionBuilder ()
postEncoinsPolicyTx = V1.postEncoinsPolicyTx

postLedgerValidatorTx :: EncoinsProtocolParams -> Integer -> TransactionBuilder ()
postLedgerValidatorTx = V1.postLedgerValidatorTx

--------------------------------------- UTXO Spending --------------------------------------------

walletSpendTx' :: Value -> TransactionBuilder (Maybe Value)
walletSpendTx' = V1.walletSpendTx'

walletSpendTx :: Value -> TransactionBuilder (Maybe Value)
walletSpendTx = V1.walletSpendTx

-- Spend utxo greater than the given value from the ENCOINS Ledger script.
ledgerSpendTx' :: EncoinsProtocolParams -> Value -> TransactionBuilder (Maybe Value)
ledgerSpendTx' = V1.ledgerSpendTx'

-- Spend utxo greater than the given value from the ENCOINS Ledger script. Fails if the utxo is not found.
ledgerSpendTx :: EncoinsProtocolParams -> Value -> TransactionBuilder (Maybe Value)
ledgerSpendTx = V1.ledgerSpendTx

----------------------------------- ENCOINS Smart Contract ---------------------------------------

encoinsBurnTx :: EncoinsProtocolParams -> [BuiltinByteString] -> EncoinsMode -> TransactionBuilder Value
encoinsBurnTx = V1.encoinsBurnTx

-- Modify the value locked in staking by the given value
ledgerModifyTx :: EncoinsProtocolParams -> Value -> TransactionBuilder ()
ledgerModifyTx = V1.ledgerModifyTx

encoinsTx :: (Address, Address) -> EncoinsProtocolParams -> EncoinsRedeemer -> EncoinsMode -> TransactionBuilder ()
encoinsTx = V1.encoinsTx