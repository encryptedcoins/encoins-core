{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module ENCOINS.Core.V1.OffChain.ENCS where

import           Data.Functor                             (($>))
import           Data.Text                                (Text)
import           Ledger.Address                           (PaymentPubKeyHash(..), stakingCredential)
import           Plutus.V2.Ledger.Api                     hiding (singleton)
import           PlutusTx.Prelude                         hiding (mapM, (<$>), (<>))

import           PlutusAppsExtra.Constraints.OffChain
import           PlutusAppsExtra.Types.Tx                 (TransactionBuilder)
import qualified Plutus.Script.Utils.Ada as P
import           Ledger.Tx.Constraints.TxConstraints (TxOutDatum (..))

delegateTx :: Address -- ^ Address of the delegator
           -> Text    -- ^ IP address of the relay-server
           -> TransactionBuilder ()
delegateTx changeAddr ipAddr = do
    let delegLabel = encodeUtf8 $ toBuiltin ("ENCS Delegation" :: Text)
        ipAddrBS   = encodeUtf8 $ toBuiltin ipAddr
    utxoProducedTx changeAddr (P.lovelaceValueOf 5_000_000) (Just $ TxOutDatumInline $ Datum $ toBuiltinData (delegLabel, ipAddrBS))
    case stakingCredential changeAddr of
        Just (StakingHash (PubKeyCredential pkh)) -> mustBeSignedByTx $ PaymentPubKeyHash pkh
        _ -> failTx "delegateTx" "The change address is not associated with a staking public key!" Nothing $> ()