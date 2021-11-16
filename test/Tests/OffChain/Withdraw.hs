{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Withdraw
Description : A test to check the behavior of the withdraw operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Withdraw where

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
tests = testGroup "offChainWithdrawnTests"
       [ withdrawTest ]

withdrawTest :: TestTree
withdrawTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "withdrawTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777))
     .&&. walletFundsChange user1Wallet
                (mainTokenValue $ -33_333_333 + 29_999_999 - totalWithdrawFees)
     .&&. walletFundsChange testRefWallet(mainTokenValue $
                      refDepositFees
                    + refWithdrawFees)
     .&&. walletFundsChange testDaoWallet
                (mainTokenValue $
                      daoDepositFees
                    + daoWithdrawFees)
     .&&. walletFundsChange testAffWallet
                (mainTokenValue $
                      affDepositFees
                    + affWithdrawFees)
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    withdrawTrace
  where
    totalDepositFees = refDepositFees + daoDepositFees + affDepositFees

    feesDepositDistribution = depositFees 33_333_333 testOperationSettings

    refDepositFees, daoDepositFees, affDepositFees :: Integer
    refDepositFees = refillFees feesDepositDistribution
    daoDepositFees = daoFees    feesDepositDistribution
    affDepositFees = affFees    feesDepositDistribution

    totalWithdrawFees = refWithdrawFees + daoWithdrawFees + affWithdrawFees

    feesWithdrawDistribution = withdrawFees 29_999_999 testOperationSettings

    refWithdrawFees, daoWithdrawFees, affWithdrawFees :: Integer
    refWithdrawFees = refillFees feesWithdrawDistribution
    daoWithdrawFees = daoFees    feesWithdrawDistribution
    affWithdrawFees = affFees    feesWithdrawDistribution

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript

    userScript :: Value
    userScript =
        testUserNFT <> mainTokenValue (33_333_333 - totalDepositFees - 29_999_999)

    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue 7_777_777

withdrawTrace :: EmulatorTrace ()
withdrawTrace = do
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
    callEndpoint @"withdraw" hUser1 (MicroToken 29_999_999)
    void $ waitNSlots 10
