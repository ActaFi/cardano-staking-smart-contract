{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Unregister
Description : Tests to check the behavior of the unregister operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Unregister where

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
tests = testGroup "offChainUnregisterTests"
       [ unregisterTest
       , failedUnregisterTest
       ]

unregisterTest :: TestTree
unregisterTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "unregisterTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    unregisterTrace
  where
    scriptValueOK :: Value -> Bool
    scriptValueOK val = val ==    testStakingNFT
                               <> mainTokenValue 7_777_777

unregisterTrace :: EmulatorTrace ()
unregisterTrace = do
    hp2pWallet <- activateContractWallet p2pWallet $ runStaking
                                                     (MicroToken 7_777_777)
                                                     testStakingSettings
    pool       <- getStaking hp2pWallet
    void $ waitNSlots 2
    hUser1     <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2
    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 2
    callEndpoint @"unregister" hUser1 ()
    void $ waitNSlots 2

{-  The following test is similar to the previous one, with an added call to
    unregister, which must fail because user1 has not withdrawn all their
    staked MyToken. For this reason, the balance in the end must be exactly as
    in depositTest, which in this case means that user UTxO has been untouched.
-}
failedUnregisterTest :: TestTree
failedUnregisterTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "failedUnregisterTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777))
     .&&. walletFundsChange user1Wallet (mainTokenValue (-33_333_333))
     .&&. walletFundsChange testRefWallet (mainTokenValue refFees)
     .&&. walletFundsChange testDaoWallet (mainTokenValue dFees)
     .&&. walletFundsChange testAffWallet (mainTokenValue aFees)
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    failedUnregisterTrace
  where
    feesDistribution = depositFees 33_333_333 testOperationSettings

    refFees, dFees, aFees :: Integer
    refFees = refillFees feesDistribution
    dFees = daoFees    feesDistribution
    aFees = affFees    feesDistribution

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript

    userScript :: Value
    userScript = testUserNFT <> mainTokenValue 33_333_333 <> totalFees

    totalFees :: Value
    -- totalFees = 416_666 with a fee of 1.25% over 33_333_333
    totalFees = mainTokenValue $ negate $ refFees + dFees + aFees

    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue 7_777_777

failedUnregisterTrace :: EmulatorTrace ()
failedUnregisterTrace = do
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
    void $ waitNSlots 2
    callEndpoint @"unregister" hUser1 ()
    void $ waitNSlots 2
