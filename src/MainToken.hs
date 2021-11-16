{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|
Module      : MainToken
Description : Description of the token used by the staking pool.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module MainToken where

-- GHC libraries.
import           Data.Aeson       (FromJSON, ToJSON)
import           GHC.Generics     (Generic)
import qualified Prelude          as HP (Show (..), Eq (..))

-- Third-party libraries.
import           Ledger
import           Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude

newtype MainToken = MicroToken { getMicroToken :: Integer }
  deriving (HP.Eq, HP.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Boilerplate.
instance Eq MainToken where
    {-# INLINABLE (==) #-}
    am1 == am2 = getMicroToken am1 == getMicroToken am2

mainTokenSymbol :: CurrencySymbol
mainTokenSymbol = "ff"

mainToken :: TokenName
mainToken = "MyToken"

mainTokenAC :: AssetClass
mainTokenAC = AssetClass (mainTokenSymbol, mainToken)

{-# INLINABLE mainTokenValue #-}
mainTokenValue :: Integer -> Value
mainTokenValue = assetClassValue mainTokenAC

PlutusTx.unstableMakeIsData ''MainToken
