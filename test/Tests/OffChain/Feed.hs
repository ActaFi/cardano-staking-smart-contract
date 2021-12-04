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
import Ledger
import Plutus.Contract.Test
import Plutus.V1.Ledger.Api  (fromData, Data)
import Plutus.Trace.Emulator as Emulator
import Test.Tasty

-- Internal modules.
import BCExplorer            as BC (printBlockChainCFD, FromDataFunc (..))
import MainToken
import Staking.OffChain
import Staking.Validator
import Staking.Types

import Tests.TestUtils

tests :: TestTree
tests = testGroup "offChainFeedTests"
       [ feedTest ]

{-| Trace Summary:
    A staking pool is created with an initial amount of microtokens, then the
    adminWallet feed the staking pool with some microtokens.

    Trace Description:
    * adminWallet starts the staking pool with initialFounds microtokens.
    * wait 2 slots
    * adminWallet feed the staking pool with feedValue microtokens.
    * wait 10 slots
-}

-- The initial amount of microtokens on the staking pool.
initialFounds :: Integer
initialFounds = 7_777_777

-- The value to feed the staking pool
feedValue :: Integer
feedValue = 22_222_222

feedTrace :: EmulatorTrace ()
feedTrace = do
    hAdminWallet <- activateContractWallet adminWallet $
                      runStaking (MicroToken initialFounds) testStakingSettings
    void $ waitNSlots 2

    callEndpoint @"feed" hAdminWallet (MicroToken feedValue)
    void $ waitNSlots 10

    BC.printBlockChainCFD [BC.FD (fromData :: Data -> Maybe StakingDatum)]


{- Test Summary:
   Test a standard usage of the feed operation.

   In this test we check that:
     * adminWallet has initialFounds less microtokens.
     * The staking script has the NFT plus initialFounds + deedValue microtokens.
-}
feedTest :: TestTree
feedTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "feedTest"
    (     walletFundsChange adminWallet
                                (mainTokenValue (-initialFounds - feedValue)
                              <> minAda (-1))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    feedTrace
  where
    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue (initialFounds + feedValue)
                                   <> minAda 1

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == stakingScript
