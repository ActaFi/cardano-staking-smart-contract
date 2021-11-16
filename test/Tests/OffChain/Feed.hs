{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Feed
Description : A test to check the behavior of the feed operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Feed where

-- GHC libraries.
import Control.Monad

-- Third-praty libraries.
import Control.Lens
import Ledger.Value          as Value
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import Test.Tasty

-- Internal modules.
import MainToken
import Staking.OffChain
import Staking.Validator

import Tests.TestUtils

tests :: TestTree
tests = testGroup "offChainFeedTests"
       [ feedTest ]

feedTest :: TestTree
feedTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "feedTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777 - 22_222_222))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    feedTrace
  where
    scriptValueOK :: Value -> Bool
    scriptValueOK val = val ==    testStakingNFT
                               <> mainTokenValue (7_777_777 + 22_222_222)

feedTrace :: EmulatorTrace ()
feedTrace = do
    hp2pWallet <- activateContractWallet p2pWallet $ runStaking
                                                     (MicroToken 7_777_777)
                                                     testStakingSettings
    void $ Emulator.waitNSlots 2
    callEndpoint @"feed" hp2pWallet (MicroToken 22_222_222)
    void $ Emulator.waitNSlots 10
