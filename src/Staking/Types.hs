{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|
Module      : Staking.Types
Description : Types used by the staking pool and to describe each UTxO.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.Types where

import           Data.Aeson              ( FromJSON
                                         , ToJSON
                                         )
import           GHC.Generics            (Generic)
import qualified Prelude          as     HP ( Show (..)
                                            , Eq (..)
                                            )

import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude

import           MainToken
import           Staking.Business

-- Characterizing the staking pool. Contract Parameter.
data Staking = Staking
    { nft       :: !AssetClass
    , settings  :: !StakingSettings
    }
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | General settings for the staking pool.
data StakingSettings = StakingSettings
    { refWallet   :: !PubKeyHash  -- ^ Wallet for refilling rewards pool
    , daoWallet   :: !PubKeyHash  -- ^ Wallet for DAO program
    , affWallet   :: !PubKeyHash  -- ^ Wallet for affiliate network
    , opSettings  :: !OperationSettings -- ^ Settings for staking operations
    }
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Datum types.
data StakingDatum =
      PoolDatum PoolState
    | UserDatum UserState
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Redeemer.
data StakingRedeemer =
      Feed       !MainToken
    | Deposit    !MainToken !POSIXTime
    | Withdraw   !MainToken !POSIXTime
    | Register   !PubKeyHash !AssetClass
    | Unregister !PubKeyHash
    | Claim      !MainToken !POSIXTime
    | Compound   !MainToken !POSIXTime
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructors for the untyped redeemers.
feedRedeemer :: MainToken -> Redeemer
feedRedeemer = Redeemer . PlutusTx.toBuiltinData . Feed

registerRedeemer :: PubKeyHash -> AssetClass -> Redeemer
registerRedeemer pkh = Redeemer . PlutusTx.toBuiltinData . Register pkh

unregisterRedeemer :: PubKeyHash -> Redeemer
unregisterRedeemer = Redeemer . PlutusTx.toBuiltinData . Unregister

depositRedeemer :: MainToken -> POSIXTime -> Redeemer
depositRedeemer mt = Redeemer . PlutusTx.toBuiltinData . Deposit mt

withdrawRedeemer :: MainToken -> POSIXTime -> Redeemer
withdrawRedeemer mt = Redeemer . PlutusTx.toBuiltinData . Withdraw mt

claimRedeemer :: MainToken -> POSIXTime -> Redeemer
claimRedeemer mt = Redeemer . PlutusTx.toBuiltinData . Claim mt

compoundRedeemer :: MainToken -> POSIXTime -> Redeemer
compoundRedeemer mt = Redeemer . PlutusTx.toBuiltinData . Compound  mt

-- Helper functions.
datumIsUser :: StakingDatum -> Bool
datumIsUser (UserDatum _) = True
datumIsUser _             = False

datumIsPool :: StakingDatum -> Bool
datumIsPool (PoolDatum _) = True
datumIsPool _             = False

mkPoolDatum :: [(PubKeyHash, AssetClass)] -> StakingDatum
mkPoolDatum = PoolDatum . mkPoolState

mkUserDatum ::
       PubKeyHash
    -> [Deposit]
    -> Maybe POSIXTime
    -> StakingDatum
mkUserDatum pkh dep lc = UserDatum $ mkUserState pkh dep lc

mkStaking ::
       AssetClass
    -> StakingSettings
    -> Staking
mkStaking ac s = Staking { nft = ac, settings = s }

instance Eq StakingDatum where
    {-# INLINABLE (==) #-}
    PoolDatum pd1 == PoolDatum pd2 = pd1 == pd2
    UserDatum us1 == UserDatum us2 = us1 == us2
    _             == _             = False

instance Eq StakingRedeemer where
    {-# INLINABLE (==) #-}
    Feed       am1      == Feed       am2      = am1 == am2
    Deposit    am1 t1   == Deposit    am2 t2   = am1 == am2 && t1 == t2
    Withdraw   am1 t1   == Withdraw   am2 t2   = am1 == am2 && t1 == t2
    Register   pkh1 ac1 == Register   pkh2 ac2 = pkh1 == pkh2 && ac1 == ac2
    Unregister pkh1     == Unregister pkh2     = pkh1 == pkh2
    Claim      am1 t1   == Claim      am2 t2   = am1 == am2 && t1 == t2
    Compound   am1 t1   == Compound   am2 t2   = am1 == am2 && t1 == t2
    _ == _ = False

PlutusTx.makeLift ''StakingSettings
PlutusTx.makeLift ''Staking
PlutusTx.unstableMakeIsData ''StakingDatum
PlutusTx.unstableMakeIsData ''StakingRedeemer
