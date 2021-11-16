{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Staking.Tokens
Description : Name of the StakingNFT and UserNFT tokens.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.Tokens where

import Ledger

stakingNFTName :: TokenName
stakingNFTName = "Staking"

userNFTName :: TokenName
userNFTName = "User"
