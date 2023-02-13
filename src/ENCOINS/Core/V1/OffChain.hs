{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Core.V1.OffChain where

import           Control.Monad.State                        (when, gets)
import           Data.Bool                                  (bool)
import           Data.Functor                               (($>), (<$>))
import           Data.Maybe                                 (fromJust, catMaybes)
import           Data.Text                                  (pack)
import           Ledger                                     (DecoratedTxOut(..), _decoratedTxOutAddress)
import           Ledger.Ada                                 (lovelaceValueOf)
import           Ledger.Address                             (toPubKeyHash, stakingCredential)
import           Ledger.Tokens                              (token)
import           Ledger.Value                               (AssetClass (..), geq, isAdaOnlyValue, gt, lt)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                           hiding ((<$>), (<>), mapM)
import           Prelude                                    ((<>), mapM, show)
import           Text.Hex                                   (decodeHex, encodeHex)

import           ENCOINS.BaseTypes                          (MintingPolarity (..))
import           ENCOINS.Bulletproofs                       (polarityToInteger)
import           ENCOINS.Core.V1.OnChain
import           PlutusAppsExtra.Constraints.OffChain
import           PlutusAppsExtra.Scripts.CommonValidators   (alwaysFalseValidatorAddress)
import           PlutusAppsExtra.Scripts.OneShotCurrency    (oneShotCurrencyMintTx)
import           PlutusAppsExtra.Types.Tx                   (TransactionBuilder, TxConstructor (..))

verifierPKH ::BuiltinByteString
verifierPKH = toBuiltin $ fromJust $ decodeHex "FA729A50432E19737EEEEA0BFD8E673D41973E7ACE17A2EEDB2119F6F989108A"

relayFee :: Value
relayFee = lovelaceValueOf 2_000_000

------------------------------------- Beacon Minting Policy --------------------------------------

beaconMintTx :: TxOutRef -> TransactionBuilder ()
beaconMintTx ref = oneShotCurrencyMintTx (beaconParams ref) $> ()

beaconSendTx :: TxOutRef -> TransactionBuilder ()
beaconSendTx ref = utxoProducedScriptTx vh Nothing v ()
  where vh = stakingValidatorHash $ encoinsSymbol (beaconCurrencySymbol ref, verifierPKH)
        v  = beaconToken ref + lovelaceValueOf 2_000_000

beaconTx :: TxOutRef -> TransactionBuilder ()
beaconTx ref = do
    beaconMintTx ref
    beaconSendTx ref

----------------------------------- ENCOINS Minting Policy ---------------------------------------

type EncoinsRedeemerWithData = (Address, EncoinsRedeemer)

encoinsBurnTx :: EncoinsParams -> [BuiltinByteString] -> TransactionBuilder ()
encoinsBurnTx _   []       = return ()
encoinsBurnTx par (bs:bss) = do
    let f = \_ o -> _decoratedTxOutValue o `geq` encoin par bs
    res1 <- utxoSpentPublicKeyTx' f
    res2 <- utxoSpentScriptTx' f (const . const $ ledgerValidatorV) (const . const $ ())
    -- At most one of the results is a Just.
    case bool res2 res1 (isJust res1) of
      Nothing -> failTx "encoinsBurnTx" ("Cannot find the required coin: " <> encodeHex (fromBuiltin bs)) Nothing $> ()
      Just (_, o) -> -- Filter out all encoins in the output and continue
        let bss' = filter (`notElem` encoinsInValue par (_decoratedTxOutValue o)) bss
        in encoinsBurnTx par bss'

encoinsTx :: EncoinsParams -> EncoinsRedeemerWithData -> TransactionBuilder ()
encoinsTx par@(beaconSymb, _) (_, red@(addr, (v, inputs), _, _))  = do
    let beacon      = token (AssetClass (beaconSymb, beaconTokenName))
        coinsToBurn = filter (\(_, p) -> p == Burn) inputs
        val         = lovelaceValueOf (v * 1_000_000)
        valEncoins  = sum $ map (\(bs, p) -> scale (polarityToInteger p) (encoin par bs)) inputs
    encoinsBurnTx par $ map fst coinsToBurn
    tokensMintedTx (encoinsPolicyV par) red valEncoins
    stakingModifyTx (encoinsSymbol par) val 1
    when (v > 0) $
        utxoReferencedTx (\_ o -> _decoratedTxOutAddress o == addr && _decoratedTxOutValue o `geq` beacon) $> ()
    when (v < 0) $ fromMaybe (failTx "encoinsTx" "The address in the redeemer is not locked by a public key." Nothing $> ()) $ do
        pkh <- toPubKeyHash addr
        return $ utxoProducedPublicKeyTx pkh (stakingCredential addr) (negate val) (Nothing :: Maybe ())

postEncoinsPolicyTx :: EncoinsParams -> Integer -> TransactionBuilder ()
postEncoinsPolicyTx par salt = postMintingPolicyTx (alwaysFalseValidatorAddress salt) (encoinsPolicyV par) (Just ()) zero

------------------------------------- ADA Staking Validator --------------------------------------

-- Spend utxo greater than the given value from the Staking script.
stakingSpendTx' :: StakingParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx' par val =
    fmap (_decoratedTxOutValue . snd) <$> utxoSpentScriptTx'
        (\_ o -> _decoratedTxOutValue o `geq` val
        && isAdaOnlyValue (_decoratedTxOutValue o)
        && _decoratedTxOutAddress o == stakingValidatorAddress par)
        (const . const $ stakingValidatorV par) (const . const $ ())

-- Spend utxo greater than the given value from the Staking script. Fails if the utxo is not found.
stakingSpendTx :: StakingParams -> Value -> TransactionBuilder (Maybe Value)
stakingSpendTx par val = do
    res <- stakingSpendTx' par val
    if isNothing res
        then do
            utxos <- gets txConstructorLookups
            failTx "stakingSpendTx" ("Cannot find a suitable utxo to spend. UTXOs: " <> pack (show utxos)) res
        else return res

-- Combines several utxos into one.
stakingCombineTx :: StakingParams -> Value -> Integer -> TransactionBuilder ()
stakingCombineTx par val 0 = utxoProducedScriptTx (stakingValidatorHash par) Nothing val ()
stakingCombineTx par val n = do
    res <- stakingSpendTx par zero
    let val' = val + fromMaybe zero res
    stakingCombineTx par val' (n-1)

-- Modify the value locked in staking by the given value
stakingModifyTx :: StakingParams -> Value -> Integer -> TransactionBuilder ()
stakingModifyTx par val n
    | val `gt` zero = utxoProducedScriptTx (stakingValidatorHash par) Nothing val ()
    | otherwise      = when (val `lt` zero) $ do
        res  <- stakingSpendTx par (negate val)
        res' <- mapM (const $ stakingSpendTx' par zero) [1..n] -- Spend `n` additional utxos
        when (isJust res) $
            let valSpent  = fromJust res + sum (catMaybes res')
                valChange = valSpent + val
            in when (valChange `gt` zero) $ utxoProducedScriptTx (stakingValidatorHash par) Nothing valChange ()

postStakingValidatorTx :: StakingParams -> Integer -> TransactionBuilder ()
postStakingValidatorTx par salt = postValidatorTx (alwaysFalseValidatorAddress salt) (stakingValidatorV par) (Just ()) zero

------------------------------------- ENCOINS Ledger Validator -----------------------------------------

ledgerTx :: EncoinsParams -> [BuiltinByteString] -> TransactionBuilder ()
ledgerTx par gs =
    let vals = map (encoin par) gs
    in mapM_ (\val -> utxoSpentScriptTx (\_ o -> _decoratedTxOutValue o `geq` val)
        (const . const $ ledgerValidatorV) (const . const $ ())) vals
        