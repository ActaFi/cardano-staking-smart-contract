{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Claim
Description : A test to verify the behavior of the claim operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Claim where

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
import Staking.Business.User
import Staking.Business
import Staking.OffChain
import Staking.Validator

import Tests.TestUtils

tests :: TestTree
tests = testGroup "offChainClaimTests"
       [ claimTest ]

claimTest :: TestTree
claimTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "claimTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777))
     .&&. walletFundsChange user1Wallet (mainTokenValue (-33_333_333 + 15))
     .&&. walletFundsChange testRefWallet (mainTokenValue refDepositFees)
     .&&. walletFundsChange testDaoWallet (mainTokenValue daoDepositFees)
     .&&. walletFundsChange testAffWallet (mainTokenValue affDepositFees)
     .&&. valueAtAddress (addressStaking testClaimStaking) scriptValueOK
    )
    claimTrace
  where
    -- totalDepositFees = 249_999 with a fee of 0.75% over 33_333_333
    totalDepositFees = refDepositFees + daoDepositFees + affDepositFees

    -- daoDepositFees =  49_999 (20% of 416_666)
    -- affDepositFees =  74_999 (30% of 416_666)
    -- refDepositFees = 125_001 (the rest)
    feesDepositDistribution = depositFees 33_333_333 testOperationSettings

    refDepositFees, daoDepositFees, affDepositFees :: Integer
    refDepositFees = refillFees feesDepositDistribution
    daoDepositFees = daoFees    feesDepositDistribution
    affDepositFees = affFees    feesDepositDistribution

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript

    userScript :: Value
    userScript = testUserNFT <> mainTokenValue (33_333_333 - totalDepositFees)

    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue (7_777_777 - 15)

claimTrace :: EmulatorTrace ()
claimTrace = do
    hp2pWallet <- activateContractWallet p2pWallet $ runStaking
                                                     (MicroToken 7_777_777)
                                                     testStakingClaimSettings
    pool       <- getStaking hp2pWallet
    void $ waitNSlots 2
    hUser1     <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2
    -- register at POSIXTime 1596059098999
    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 2
    -- deposit at POSIXTime 1596059100999 (2 seconds later)
    callEndpoint @"deposit" hUser1 (MicroToken 33_333_333)
    void $ waitNSlots 100
    -- claim at POSIXTime 1596059200999 (100 seconds later)
    callEndpoint @"claim" hUser1 ()
    void $ waitNSlots 2
