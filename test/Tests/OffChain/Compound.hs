{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Compound
Description : A test to check the behavior of the compound operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Compound where

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
tests = testGroup "offChainCompoundTests"
       [ compoundTest ]

compoundTest :: TestTree
compoundTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "compoundTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777))
     .&&. walletFundsChange testRefWallet (mainTokenValue refDepositFees)
     .&&. walletFundsChange testDaoWallet (mainTokenValue daoDepositFees)
     .&&. walletFundsChange testAffWallet (mainTokenValue affDepositFees)
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    compoundTrace
  where
    totalDepositFees = refDepositFees + daoDepositFees + affDepositFees

    feesDepositDistribution = depositFees 33_333_333 testOperationSettings

    refDepositFees, daoDepositFees, affDepositFees :: Integer
    refDepositFees = refillFees feesDepositDistribution
    daoDepositFees = daoFees    feesDepositDistribution
    affDepositFees = affFees    feesDepositDistribution

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript

    userScript :: Value
    userScript =    testUserNFT
                 -- 32_916_667 = 33_333_333 minus 1.25% fee
                 <> mainTokenValue (33_333_333 - totalDepositFees + rews)

    stakingScript :: Value
    stakingScript =    testStakingNFT
                    <> mainTokenValue (7_777_777 - rews)

    rews :: Integer
    rews = 15

compoundTrace :: EmulatorTrace ()
compoundTrace = do
    hp2pWallet <- activateContractWallet p2pWallet $ runStaking
                                                     (MicroToken 7_777_777)
                                                     testStakingSettings
    pool       <- getStaking hp2pWallet
    void $ waitNSlots 2
    hUser1     <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2
    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 2
    callEndpoint @"deposit" hUser1 (MicroToken 33_333_333)
    void $ waitNSlots 100
    callEndpoint @"compound" hUser1 ()
    void $ waitNSlots 2
