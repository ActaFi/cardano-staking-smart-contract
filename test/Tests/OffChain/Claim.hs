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
import Ledger
import Plutus.Contract.Test
import Plutus.V1.Ledger.Api  (fromData, Data)
import Plutus.Trace.Emulator as Emulator
import Test.Tasty

-- Internal modules.
import BCExplorer            as BC (printBlockChainCFD, FromDataFunc (..))
import MainToken
import Staking.Business
import Staking.OffChain
import Staking.Validator
import Staking.Types

import Tests.TestUtils

tests :: TestTree
tests = testGroup "offChainClaimTests"
       [ claimTest ]

{-| Trace Summary:
    A staking pool is created with an initial amount of microtokens, a user
    registers into the staking pool, deposits some microtokens, wait a while and
    claim rewards.

    Trace Description:
    * adminWallet starts the staking pool with initialFounds microtokens.
    * wait 2 slots
    * user1Wallet registers into the staking pool.
    * wait 2 slots
    * user1Wallet deposits depositValue microtokens.
    * wait 100 slots
    * user1Wallet claims
    * wait 10 slots
-}

-- The initial amount of microtokens on the staking pool.
initialFounds :: Integer
initialFounds = 7_777_777

-- The value to be deposited by the user into the staking pool.
depositValue :: Integer
depositValue = 33_333_333

-- The expected rewards to be claimed.
rews :: Integer
rews = 15

claimTrace :: EmulatorTrace ()
claimTrace = do
    hAdminWallet <- activateContractWallet adminWallet $
                      runStaking (MicroToken initialFounds) testStakingSettings
    pool <- getStaking hAdminWallet
    void $ waitNSlots 2

    hUser1 <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2

    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 2

    callEndpoint @"deposit" hUser1 (MicroToken depositValue)
    void $ waitNSlots 100

    callEndpoint @"claim" hUser1 ()
    void $ waitNSlots 10

    BC.printBlockChainCFD [BC.FD (fromData :: Data -> Maybe StakingDatum)]


{- Test Summary:
   Test a standard usage of the claim operation.

   In this test we check that:
     * adminWallet has initialFounds less microtokens.
     * user1Wallet  has depositValue microtokens less, but rews microtokens more.
     * refWallet has refDepositFee microtokens more.
     * daoWallet has daoDepositFee microtokens more.
     * affWallet has affDepositFee microtokens more.
     * The staking script has the NFT plus initialFounds - rews + depositValue
       - depositFees microtokens.
-}
claimTest :: TestTree
claimTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "claimTest"
    (     walletFundsChange adminWallet   adminWalletChangeValue
     .&&. walletFundsChange user1Wallet   user1WalletChangeValue
     .&&. walletFundsChange testRefWallet refWalletChangeValue
     .&&. walletFundsChange testDaoWallet daoWalletChangeValue
     .&&. walletFundsChange testAffWallet affWalletChangeValue
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    claimTrace
  where
    user1State :: UserState
    user1State = mkUserState user1WalletPKH [] Nothing

    depositFeesDistribution :: FeesDistribution
    Just (_, depositFeesDistribution) =
        deposit user1State depositValue 0 testOperationSettings

    -- The distribution of fees given the deposit of 33_333_333 microtokens.
    -- daoDepositFees =  49_999 (20% of 249_999).
    -- affDepositFees =  74_999 (30% of 249_999).
    -- refDepositFees = 125_001 (the rest).
    refDepositFees, daoDepositFees, affDepositFees :: Integer
    refDepositFees = refillFees depositFeesDistribution
    daoDepositFees = daoFees    depositFeesDistribution
    affDepositFees = affFees    depositFeesDistribution

    -- totalDepositFees = 249_999 with a fee of 0.75% over 33_333_333.
    depositFees :: Integer
    depositFees = refDepositFees + daoDepositFees + affDepositFees

    userScript :: Value
    userScript =
        testUserNFT <> mainTokenValue (depositValue - depositFees) <> minAda 1

    stakingScript :: Value
    stakingScript =
        testStakingNFT <> mainTokenValue (initialFounds - rews) <> minAda 1

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript

    adminWalletChangeValue :: Value
    adminWalletChangeValue = mainTokenValue (-initialFounds)
                          <> minAda (-1)

    user1WalletChangeValue :: Value
    user1WalletChangeValue = mainTokenValue (-depositValue + rews)
                          <> minAda (-4)

    refWalletChangeValue :: Value
    refWalletChangeValue = mainTokenValue refDepositFees <> minAda 1

    daoWalletChangeValue :: Value
    daoWalletChangeValue = mainTokenValue daoDepositFees <> minAda 1

    affWalletChangeValue :: Value
    affWalletChangeValue = mainTokenValue affDepositFees <> minAda 1
