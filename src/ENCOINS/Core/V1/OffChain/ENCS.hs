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

import           Data.Functor                         (($>))
import           Data.Text                            (Text)
import           Ledger.Address                       (stakingCredential, PaymentPubKeyHash (..))
import           Plutus.V2.Ledger.Api                 hiding (singleton)
import           PlutusTx.Prelude                     hiding (mapM, (<$>))

import           Ledger.Tx.Constraints.TxConstraints  (TxOutDatum (..))
import qualified Plutus.Script.Utils.Ada              as P
import           Plutus.Script.Utils.Value            (singleton)
import           PlutusAppsExtra.Constraints.OffChain
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder)

delegateTx :: CurrencySymbol
           -> TokenName
           -> Address -- ^ Address of the delegator
           -> Text    -- ^ IP address of the relay-server
           -> TransactionBuilder ()
delegateTx cs tokenName changeAddr ipAddr = do
    let delegLabel1 = encodeUtf8 $ toBuiltin ("ENCOINS" :: Text)
        delegLabel2 = encodeUtf8 $ toBuiltin ("Delegate" :: Text)
        ipAddrBS    = encodeUtf8 $ toBuiltin ipAddr
    pkhBS <- case stakingCredential changeAddr of
        Just (StakingHash (PubKeyCredential (PubKeyHash pkh))) -> return pkh
        _ -> failTx "delegateTx" "The change address is not associated with a staking public key!" Nothing $> ""
    let dat = TxOutDatumInline $ Datum $ toBuiltinData [delegLabel1, delegLabel2, pkhBS, ipAddrBS]
    utxoProducedTx changeAddr (P.lovelaceValueOf 5_000_000 <> singleton cs tokenName 1) (Just dat)
    mustBeSignedByTx (PaymentPubKeyHash (PubKeyHash pkhBS))