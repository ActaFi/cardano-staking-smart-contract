{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Start
Description : A test to check the behavior of the start operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Start where

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
tests = testGroup "offChainStartTests"
       [ startTest ]

{-| Trace Summary:
    A staking pool is created with an initial amount of microtokens.

    Trace Description:
    * adminWallet starts the staking pool with initialFounds microtokens.
    * wait 10 slots
-}

-- The initial amount of microtokens on the staking pool.
initialFounds :: Integer
initialFounds = 7_777_777

startTrace :: EmulatorTrace ()
startTrace = do
    void $ activateContractWallet adminWallet $ runStaking
                                                (MicroToken initialFounds)
                                                testStakingSettings
    void $ waitNSlots 10

    BC.printBlockChainCFD [BC.FD (fromData :: Data -> Maybe StakingDatum)]


{- Test Summary:
   Test a standard usage of the start operation to initialize a staking pool.

   In this test we check that:
     * adminWallet has initialFounds less microtokens.
     * The staking script has the NFT plus initialFounds microtokens.
-}
startTest :: TestTree
startTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "startTest"
    (     walletFundsChange adminWallet (mainTokenValue (-initialFounds)
                                      <> minAda (-1))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    startTrace
  where
    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue initialFounds <> minAda 1

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == stakingScript
