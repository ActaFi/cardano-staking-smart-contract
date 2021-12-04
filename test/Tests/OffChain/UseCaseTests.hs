{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Tests.OffChain.UseCaseTests where

-- GHC libraries.
import Control.Lens
import Control.Monad
import Ledger.Value          as Value
import Plutus.Trace.Emulator as Emulator

-- Third-praty libraries.
import Plutus.Contract.Test
import Test.Tasty
import Plutus.V1.Ledger.Api  (fromData, Data)

-- Internal modules.
import BCExplorer            as BC ( printBlockChainCFD
                                   , FromDataFunc (..)
                                   )
import MainToken
import Staking.Business
import Staking.OffChain
import Staking.Validator
import Staking.Types

import Tests.TestUtils

tests :: TestTree
tests = testGroup "useCaseTests"
       [ useCaseTest0
       , useCaseTest1
       , useCaseTest2
       ]

-- Claim nothing due to not reaching the minimum amount
useCaseTest0 :: TestTree
useCaseTest0 = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "Test 0"
    (    walletFundsChange adminWallet   (mainTokenValue (-7_777_777))
    .&&. walletFundsChange user1Wallet   (mainTokenValue (-100_000_000))
    .&&. walletFundsChange testRefWallet (mainTokenValue refDepositFees)
    .&&. walletFundsChange testDaoWallet (mainTokenValue daoDepositFees)
    .&&. walletFundsChange testAffWallet (mainTokenValue affDepositFees)
    .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    useCaseTrace0
  where
    totalDepositFees = refDepositFees + daoDepositFees + affDepositFees

    feesDepositDistribution = depositFees 100_000_000 testOperationSettings

    refDepositFees, daoDepositFees, affDepositFees :: Integer
    refDepositFees = refillFees feesDepositDistribution
    daoDepositFees = daoFees    feesDepositDistribution
    affDepositFees = affFees    feesDepositDistribution

    scriptValueOK :: Value -> Bool
    scriptValueOK val = val == userScript <> stakingScript
