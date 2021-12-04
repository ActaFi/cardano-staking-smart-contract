{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Register
Description : A test to check the behavior of the register operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Register where

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
tests = testGroup "offChainRegisterTests"
       [ registerTest ]

{-| Trace Summary:
    A staking pool is created with an initial amount of microtokens and a user
    registers into the staking pool.

    Trace Description:
    * adminWallet starts the staking pool with initialFounds microtokens.
    * wait 2 slots
    * user1Wallet registers into the staking pool.
    * wait 10 slots
-}

-- The initial amount of microtokens on the staking pool.
initialFounds :: Integer
initialFounds = 7_777_777

registerTrace  :: EmulatorTrace ()
registerTrace = do
    hAdminWallet <- activateContractWallet adminWallet $
                      runStaking (MicroToken initialFounds) testStakingSettings
    pool <- getStaking hAdminWallet
    void $ waitNSlots 2

    hUser1 <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2

    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 10

    BC.printBlockChainCFD [BC.FD (fromData :: Data -> Maybe StakingDatum)]


{- Test Summary:
   Test a standard usage of the register operation.

   In this test we check that:
     * adminWallet has initialFounds less microtokens.
     * The staking script has the NFT plus initialFounds microtokens.
-}
registerTest :: TestTree
registerTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "registerTest"
    (     walletFundsChange adminWallet (mainTokenValue (-initialFounds)
                                      <> minAda (-1))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    registerTrace
  where
    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue initialFounds <> minAda 1

    userScript :: Value
    userScript = testUserNFT <> minAda 1

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript
