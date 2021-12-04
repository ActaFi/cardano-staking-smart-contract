{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Utils.ScriptContext
Description : Useful functions about script contexts.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop

Common functions for manipulating script context.
-}

module Utils.ScriptContext where

import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..)
                                             , unless, mapMaybe, find
                                             )
import           Ledger               hiding (singleton)
import           Ledger.Value         as Value

-- | Gets all utxo inputs corresponding with address addr.
{-# INLINABLE getTxInputs #-}
getTxInputs :: Address -> ScriptContext -> [TxInInfo]
getTxInputs addr ctx = [ i | i <- txInfoInputs info
                           , txOutAddress (txInInfoResolved i) == addr
                       ]
  where
    info = scriptContextTxInfo ctx

-- | Gets all utxo outputs corresponding with address addr.
{-# INLINABLE getTxOutputs #-}
getTxOutputs :: Address -> ScriptContext -> [TxOut]
getTxOutputs addr ctx = [ o | o <- txInfoOutputs info
                            , txOutAddress o == addr
                        ]
  where
    info = scriptContextTxInfo ctx

-- | Gets all utxo inputs with script addresses.
{-# INLINABLE getAllScriptInputs #-}
getAllScriptInputs :: ScriptContext -> [TxInInfo]
getAllScriptInputs =
  filterInputs (isJust . toValidatorHash . txOutAddress . txInInfoResolved)

-- | Gets all utxo inputs with public key addresses.
{-# INLINABLE getAllWalletInputs #-}
getAllWalletInputs :: ScriptContext -> [TxInInfo]
getAllWalletInputs =
  filterInputs (isJust . toValidatorHash . txOutAddress . txInInfoResolved)

-- | Filter utxo inputs.
{-# INLINABLE filterInputs #-}
filterInputs :: (TxInInfo -> Bool) -> ScriptContext -> [TxInInfo]
filterInputs f = filter f . (txInfoInputs . scriptContextTxInfo)

-- | Obtains the unique script utxo which has the corresponding NFT.
getTxInFromNFT :: ScriptContext -> AssetClass -> Maybe TxInInfo
getTxInFromNFT ctx nft =
    case filterInputs (checkTxHasNFT nft . txInInfoResolved) ctx of
        [o] -> Just o
        _   -> Nothing

-- | Filter utxo outputs.
{-# INLINABLE filterOutputs #-}
filterOutputs :: (TxOut -> Bool) -> ScriptContext -> [TxOut]
filterOutputs f = filter f . (txInfoOutputs . scriptContextTxInfo)

-- | Filter all the utxo outputs that pay to the same script address
--   that we are currently spending from.
{-# INLINABLE filterContinuingOutputs #-}
filterContinuingOutputs
    :: (TxOut -> Bool)
    -> ScriptContext
    -> [TxOut]
filterContinuingOutputs f = filter f . getContinuingOutputs

-- | Obtains the unique script utxo which has the corresponding NFT.
getTxOutFromNFT :: ScriptContext -> AssetClass -> Maybe TxOut
getTxOutFromNFT ctx nft =
    case filterContinuingOutputs (checkTxHasNFT nft) ctx of
        [o] -> Just o
        _   -> Nothing

-- | Checks that a transaction has attached the specified NFT.
{-# INLINABLE checkTxHasNFT #-}
checkTxHasNFT :: AssetClass -> TxOut -> Bool
checkTxHasNFT asc o = assetClassValueOf (txOutValue o) asc == 1

-- | Checks that the own script input contains the specified NFT.
{-# INLINABLE inputHasNFT #-}
inputHasNFT :: AssetClass -> ScriptContext -> Bool
inputHasNFT asc =
  maybe (traceError "script input missing")
        (checkTxHasNFT asc . txInInfoResolved) . findOwnInput

-- | Checks that the own script output contains the specified NFT.
{-# INLINABLE outputHasNFT #-}
outputHasNFT :: AssetClass -> ScriptContext -> Bool
outputHasNFT asc = maybe (traceError "script unique own output missing")
                         (checkTxHasNFT asc) . uniqueScriptOutput

-- | Gets the datum attached to a utxo.
{-# INLINABLE getTxDatum #-}
getTxDatum :: PlutusTx.FromData d => TxOut -> ScriptContext -> Maybe d
getTxDatum o ctx = txOutDatum o >>= (`findDatum` scriptContextTxInfo ctx)
                   >>= PlutusTx.fromBuiltinData . getDatum

-- | Obtains the unique script utxo if there exists.
{-# INLINABLE uniqueScriptOutput #-}
uniqueScriptOutput :: ScriptContext -> Maybe TxOut
uniqueScriptOutput ctx = case getContinuingOutputs ctx of
                           [o] -> Just o
                           _   -> Nothing

-- | Check the value of the unique script input and output doesn't changed.
{-# INLINABLE unchangedValueFromNFT #-}
unchangedValueFromNFT :: ScriptContext -> AssetClass -> Bool
unchangedValueFromNFT ctx asc =
    traceIfFalse "unchangedValueFromNFT: Changed value" $ inVal == outVal
  where
    inVal :: Value
    inVal = maybe
            (traceError "unchangedValueFromNFT inVal: missing utxo input")
            (txOutValue . txInInfoResolved)
            (findOwnInput ctx)

    outVal :: Value
    outVal = maybe
             (traceError "unchangedValueFromNFT outVal: missing utxo output")
             txOutValue
             (getTxOutFromNFT ctx asc)

-- | Check the value of the unique script input and output doesn't changed.
{-# INLINABLE unchangedValue #-}
unchangedValue :: ScriptContext -> Bool
unchangedValue ctx =
    traceIfFalse "unchangedValue: Changed value" $ inVal == outVal
  where
    inVal :: Value
    inVal = maybe (traceError "unchangedValue inVal: missing utxo input")
                  (txOutValue . txInInfoResolved)
                  (findOwnInput ctx)

    outVal :: Value
    outVal = maybe (traceError "unchangedValue outVal: missing utxo output")
                   txOutValue
                   (uniqueScriptOutput ctx)

-- | Obtains the UTxO being spent with the corresponding NFT.
{-# INLINABLE findSpentTxOut #-}
findSpentTxOut :: ScriptContext -> AssetClass -> Maybe TxOut
findSpentTxOut ctx ac = txInInfoResolved <$> getTxInFromNFT ctx ac
