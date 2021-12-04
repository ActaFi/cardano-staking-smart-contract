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
tests = testGroup "offChainUnregisterTests"
       [ unregisterTest
       , failedUnregisterTest
       ]

{-| Trace Summary:
    A staking pool is created with an initial amount of microtokens, a user
    registers into the staking pool, deposits some microtokens, and unregister
    from the staking pool successfully.

    Trace Description:
    * adminWallet starts the staking pool with initialFounds microtokens.
    * wait 2 slots.
    * user1Wallet registers into the staking pool.
    * wait 2 slots.
    * user1Wallet unregisters from the staking pool.
    * wait 10 slots.
-}

-- The initial amount of microtokens on the staking pool.
initialFounds :: Integer
initialFounds = 7_777_777

unregisterTrace :: EmulatorTrace ()
unregisterTrace = do
    hAdminWallet <- activateContractWallet adminWallet $
                      runStaking (MicroToken initialFounds) testStakingSettings
    pool <- getStaking hAdminWallet
    void $ waitNSlots 2

    hUser1 <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2

    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 2

    callEndpoint @"unregister" hUser1 ()
    void $ waitNSlots 10

    BC.printBlockChainCFD [BC.FD (fromData :: Data -> Maybe StakingDatum)]

{- Test Summary:
   Test a standard usage of the unregister operation.

   In this test we check that:
     * adminWallet has initialFounds less microtokens.
     * The staking script has the NFT plus initialFounds microtokens.
-}
unregisterTest :: TestTree
unregisterTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "unregisterTest"
    (     walletFundsChange adminWallet (mainTokenValue (-initialFounds)
                                     <> minAda (-1))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    unregisterTrace
  where
    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue initialFounds <> minAda 1

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == stakingScript


{-| Trace Summary:
    A staking pool is created with an initial amount of microtokens, a user
    registers into the staking pool, deposits some microtokens, and try to
    unregister from the staking pool without success.

    Trace Description:
    * adminWallet starts the staking pool with initialFounds microtokens.
    * wait 2 slots.
    * user1Wallet registers into the staking pool.
    * wait 2 slots.
    * user1Wallet deposits depositValue microtokens.
    * wait 2 slots.
    * user1Wallet try to unregister from the staking pool.
    * wait 10 slots.
-}

-- The value deposited by the user into the staking pool.
depositValue :: Integer
depositValue = 33_333_333

failedUnregisterTrace :: EmulatorTrace ()
failedUnregisterTrace = do
    hAdminWallet <- activateContractWallet adminWallet $
                      runStaking (MicroToken initialFounds) testStakingSettings
    pool <- getStaking hAdminWallet
    void $ waitNSlots 2

    hUser1 <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2

    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 2

    callEndpoint @"deposit" hUser1 (MicroToken depositValue)
    void $ waitNSlots 2

    callEndpoint @"unregister" hUser1 ()
    void $ waitNSlots 10

    BC.printBlockChainCFD [BC.FD (fromData :: Data -> Maybe StakingDatum)]


{- Test Summary:
    The following test is similar to the previous one, with an added call to
    deposit, which must fail because user 1 has not withdrawn all their staked
    MyToken. For this reason, the balance in the end must be exactly as in
    depositTest, which in this case means that user UTxO has been untouched.

   In this test we check that:
     * adminWallet has initialFounds less microtokens.
     * user1Wallet  has depositValue microtokens less.
     * refWallet has refDepositFee microtokens more.
     * daoWallet has daoDepositFee microtokens more.
     * affWallet has affDepositFee microtokens more.
     * The staking script has the NFT plus initialFounds + depositValue
       - depositFees microtokens.
-}
failedUnregisterTest :: TestTree
failedUnregisterTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "failedUnregisterTest"
    (     walletFundsChange adminWallet   adminWalletChangeValue
     .&&. walletFundsChange user1Wallet   user1WalletChangeValue
     .&&. walletFundsChange testRefWallet refWalletChangeValue
     .&&. walletFundsChange testDaoWallet daoWalletChangeValue
     .&&. walletFundsChange testAffWallet affWalletChangeValue
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    failedUnregisterTrace
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

    -- depositFees = 249_999 with a fee of 0.75% over 33_333_333.
    depositFees = refDepositFees + daoDepositFees + affDepositFees

    userScript :: Value
    userScript =
        testUserNFT <> mainTokenValue (depositValue - depositFees) <> minAda 1

    stakingScript :: Value
    stakingScript = testStakingNFT <> mainTokenValue initialFounds <> minAda 1

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript

    adminWalletChangeValue :: Value
    adminWalletChangeValue = mainTokenValue (-initialFounds)
                          <> minAda (-1)

    user1WalletChangeValue :: Value
    user1WalletChangeValue = mainTokenValue (-depositValue)
                          <> minAda (-4)

    refWalletChangeValue :: Value
    refWalletChangeValue = mainTokenValue refDepositFees <> minAda 1

    daoWalletChangeValue :: Value
    daoWalletChangeValue = mainTokenValue daoDepositFees <> minAda 1

    affWalletChangeValue :: Value
    affWalletChangeValue = mainTokenValue affDepositFees <> minAda 1
