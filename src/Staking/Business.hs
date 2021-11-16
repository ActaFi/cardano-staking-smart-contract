{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}

{-|
Module      : Staking.Business
Description : Main module of Business logic.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.Business
    (
    -- ^ Main User functions
      claim
    , compound
    , deposit
    , withdraw
    -- ^ Main Pool functions
    , register
    , unregister
    -- ^ Utils
    , validTimeRange
    , isAfter
    , getUserNFT
    , isRegistered
    , module Staking.Business.Types
    ) where

-- Third-party libraries.
import Ledger

-- Internal modules.
import Staking.Business.Pool
import Staking.Business.User
import Staking.Business.Types

validTimeRange :: POSIXTime
validTimeRange = POSIXTime 10_000
