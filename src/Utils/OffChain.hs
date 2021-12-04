{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Utils.OffChain
Description : Common off-chain functions.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Utils.OffChain where

import qualified Data.Text                 as T
import qualified Data.Map                  as Map
import           Control.Lens

import qualified PlutusTx
import           Ledger                    hiding (singleton)
import           Ledger.Value              (assetClassValueOf)
import           Plutus.Contract           as Contract
import           Plutus.Contracts.Currency as Currency

{- | Get the datum from a ChainIndexTxOut, only if it is not a datum hash. -}
getChainIndexTxOutDatum :: PlutusTx.FromData d => ChainIndexTxOut -> Maybe d
getChainIndexTxOutDatum ciTxOut =
    case matching _ScriptChainIndexTxOut ciTxOut of
        Right (_, _, Right d, _) -> PlutusTx.fromBuiltinData $ getDatum d
        _                        -> Nothing

{- | Get the value from a ChainIndexTxOut. -}
getChainIndexTxOutValue :: ChainIndexTxOut -> Value
getChainIndexTxOutValue o = o ^. ciTxOutValue

{- | Off-chain function for getting the unique UTxO for the given address that
     contains the given NFT. The ChainIndexTxOut value, if possible, has
     the datum field loaded with the correct datum result of calling
    `datumFromHash`.
-}
lookupScriptUTxO
    :: forall w s
    .  Address
    -> AssetClass
    -> Contract w s T.Text (TxOutRef, ChainIndexTxOut)
lookupScriptUTxO addr nftAC = do
  utxos <- Map.filter (checkTxHasNFT nftAC . (^. ciTxOutValue)) <$> utxosAt addr
  case Map.toList utxos of
    [(oref, o)] -> (oref,) <$> ciTxOutDatum loadDatum o
    _           -> throwError $ T.unwords
                     [ "Can't find the unique utxo of address"
                     , T.pack $ show addr
                     , "with the nft"
                     , T.pack $ show nftAC
                     ]
  where
    loadDatum
        :: Either DatumHash Datum
        -> Contract w s T.Text (Either DatumHash Datum)
    loadDatum lhd@(Left dh) = maybe lhd Right <$> datumFromHash dh
    loadDatum d = return d

    checkTxHasNFT :: AssetClass -> Value -> Bool
    checkTxHasNFT asc v = assetClassValueOf v asc == 1

-- | Utility for forging an NFT.
forgeNFT :: PubKeyHash -> TokenName -> Contract w s T.Text OneShotCurrency
forgeNFT pkh tkn = mapError (T.pack . show @Currency.CurrencyError) $
                   mintContract pkh [(tkn, 1)]
