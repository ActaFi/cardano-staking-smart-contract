{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Tests.Attacks.AttackUtils
Description : Helper functions to be used by the attacks.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.Attacks.AttackUtils where

import           Control.Monad
import qualified Data.Map                  as Map
import           Data.Text                 as T (Text, pack)

-- Third-party libraries libraries.
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import           Ledger.Value              as Value
import           Plutus.Contract           as Contract

-- Internal modules.
import           MainToken
import           Staking.Business
import           Staking.Types
import           Staking.Validator
import           Utils.OffChain

-- Helper functions.
mkLookups ::
       Staking
    -> [(TxOutRef, ChainIndexTxOut)]
    -> ScriptLookups StakingType
mkLookups p utxos =
       Constraints.typedValidatorLookups (typedValidatorStaking p)
    <> Constraints.otherScript (validatorStaking p)
    <> Constraints.unspentOutputs (Map.fromList utxos)

-- | Monadic function for getting the datum from a staking pool UTxO.
getPoolState :: ChainIndexTxOut -> Contract w s T.Text PoolState
getPoolState o =
    case getChainIndexTxOutDatum o of
        Just (PoolDatum ps) -> return ps
        Just (UserDatum _)  ->
            throwError "Expected StakingDatum but found UserDatum."
        Nothing             -> throwError "Cannot find contract datum."

-- | Monadic function for getting the UserState from a user script UTxO.
getUserState :: ChainIndexTxOut -> Contract w s T.Text UserState
getUserState o = case getChainIndexTxOutDatum o of
    Just dat -> case dat of
        PoolDatum _   ->
            throwError "Expected UserDatum but found StakingDatum."
        UserDatum res -> return res
    Nothing  -> throwError "Cannot find contract datum."

{- | Monadic function returning the user script UTxO corresponding to the
     PubKeyHash. -}
findUserUTxO
    :: Staking
    -> PubKeyHash
    -> Contract w s Text (TxOutRef, ChainIndexTxOut)
findUserUTxO staking pkh = do
    (_, oStaking) <- findStaking staking
    dat           <- getContractDatum oStaking
    case dat of
        UserDatum _             -> throwError
            "Expected StakingDatum but found UserDatum in staking script UTxO."
        PoolDatum ps ->
            case getUserNFT ps pkh of
                Just userNFT -> lookupScriptUTxO
                                    (addressStaking staking)
                                    userNFT
                _            -> throwError "Could not find user NFT."

-- | Monadic function returning the UTxO corresponding to the staking pool.
findStaking :: Staking -> Contract w s Text (TxOutRef, ChainIndexTxOut)
findStaking staking@Staking{..} = lookupScriptUTxO (addressStaking staking) nft

-- | Monadic function for getting the datum from a ChainIndexTxOut.
getContractDatum :: ChainIndexTxOut -> Contract w s T.Text StakingDatum
getContractDatum =
    maybe (throwError "Cannot find contract datum") return .
          getChainIndexTxOutDatum

{- | Monadic function for checking if there is enough funds in pool UTxO for a
     claim or compound transaction -}
checkMinFundsPoolUTxO :: Staking -> Integer -> Contract w s T.Text ()
checkMinFundsPoolUTxO staking rews = do
    (_, oStaking) <- findStaking staking
    when
      (assetClassValueOf (getChainIndexTxOutValue oStaking) mainTokenAC < rews)
      $ throwError $ pack $ "Claim or Compound transaction not issued due to "
                          ++ "unsufficient funds in pool UTxO."
