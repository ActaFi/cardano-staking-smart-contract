{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|
Module      : Staking.Business.Types
Description : The main types used to build the logic of business operations.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.Business.Types where

import           Data.Aeson              (FromJSON, ToJSON)
import           GHC.Generics            (Generic)
import qualified Prelude          as     HP (Show (..), Eq (..))

import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude

-- | The Pool State stores the information of active users in the staking pool.
newtype PoolState = PoolState { activeUsers :: [(PubKeyHash, AssetClass)] }
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq PoolState where
    {-# INLINABLE (==) #-}
    ps1 == ps2 = activeUsers ps1 == activeUsers ps2

mkPoolState :: [(PubKeyHash, AssetClass)] -> PoolState
mkPoolState aus = PoolState { activeUsers = aus }

-- | The user state consists eith the following fields:
--   user : pub key hash of the user
--   deposits : the list of deposits
--   lastClaim : the time of the last claim, if there is someone.
data UserState = UserState
    { user         :: !PubKeyHash
    , deposits     :: ![Deposit]
    , lastClaim    :: !(Maybe POSIXTime)
    }
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq UserState where
    {-# INLINABLE (==) #-}
    us1 == us2 = deposits  us1 == deposits  us2
              && lastClaim us1 == lastClaim us2

mkUserState
    :: PubKeyHash
    -> [Deposit]
    -> Maybe POSIXTime
    -> UserState
mkUserState u d l = UserState { user = u, deposits = d, lastClaim = l }

-- | Settings for the staking operations:
--   depositFee : amount of fees paid in the deposit operation (per million)
--   withdrawFee : amount of fees paid in the deposit operation (per million)
--   daoShare : % of fees for DAO program (* 1 million)
--   affShare : % of fees for affiliate network (* 1 million)
--   minDeposit : Minimum valid deposit in micro token
--   minWithdraw : Minimum valid withdraw in micro token
data OperationSettings = OperationSettings { depositFee  :: !Integer
                                           , withdrawFee :: !Integer
                                           , daoShare    :: !Integer
                                           , affShare    :: !Integer
                                           , minDeposit  :: !Integer
                                           , minWithdraw :: !Integer
                                           , minClaim    :: !Integer
                                           }
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Representation of fees to be paid by deposit and withdraw operations.
data FeesDistribution = FeesDistribution { refillFees :: !Integer
                                         , daoFees    :: !Integer
                                         , affFees    :: !Integer
                                         }
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A deposit consists on the time the deposit was performed and the amount.
type Deposit = (POSIXTime, Integer)

timeDeposit :: Deposit -> POSIXTime
timeDeposit = fst

amountDeposit :: Deposit -> Integer
amountDeposit = snd

PlutusTx.makeLift ''OperationSettings
PlutusTx.unstableMakeIsData ''PoolState
PlutusTx.unstableMakeIsData ''UserState
