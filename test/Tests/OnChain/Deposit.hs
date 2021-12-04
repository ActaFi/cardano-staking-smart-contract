{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OnChain.Deposit
Description : A test to simulate an attach on the on-chain code of deposit.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OnChain.Deposit where

-- GHC libraries.
import Control.Monad

-- Third-praty libraries.
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Scripts
import Test.Tasty

-- Internal modules.
import MainToken
import Staking

import Tests.Attacks.Deposit
import Tests.TestUtils

tests :: TestTree
tests = testGroup "onChainDepositTests"
        [ buildTest
            "depositNegativeAttackTest"
            depositAttackError
            depositAttackTrace
        , buildTest
            "depositEmptyListAttackTest"
            depositEmptyListAttackError
            depositEmptyListAttackTrace
        , buildTest
            "depositWithoutFeesAttackTest"
            depositWithoutFeesAttackError
            depositWithoutFeesAttackTrace
        ]

depositAttackTrace :: EmulatorTrace ()
depositAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                         (MicroToken 9_999_999)
                                                         testStakingSettings
    pool <- getStaking hAdminWallet
    void $ Emulator.waitNSlots 2

    hRegister <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack   <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hRegister ()
    void $ waitNSlots 2

    -- This amount will be negative inside the endpoint
    callEndpoint @"depositNegativeAttack" hAttack (MicroToken 2_222_222)
    void $ Emulator.waitNSlots 10


depositAttackError :: [ScriptError]
depositAttackError = pure $
    EvaluationError
    [ "checkUserDeposit: Amount to deposit does not reach minimum."
    , "PT5"
    ]
    "CekEvaluationFailure"

depositEmptyListAttackTrace :: EmulatorTrace ()
depositEmptyListAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                         (MicroToken 9_999_999)
                                                         testStakingSettings
    pool <- getStaking hAdminWallet
    void $ Emulator.waitNSlots 2

    hRegister <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack   <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hRegister ()
    void $ waitNSlots 2

    -- This amount won't be important because we want an empty list
    callEndpoint @"depositEmptyListAttack" hAttack (MicroToken 2_222_222)
    void $ Emulator.waitNSlots 10

depositEmptyListAttackError :: [ScriptError]
depositEmptyListAttackError = pure $
    EvaluationError
    [ "checkUserDeposit: Datum is wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"

depositWithoutFeesAttackTrace :: EmulatorTrace ()
depositWithoutFeesAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                         (MicroToken 9_999_999)
                                                         testStakingSettings
    pool <- getStaking hAdminWallet
    void $ Emulator.waitNSlots 2

    hRegister <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack   <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hRegister ()
    void $ waitNSlots 2

    callEndpoint @"depositWithoutFeesAttack" hAttack (MicroToken 2_222_222)
    void $ Emulator.waitNSlots 10

depositWithoutFeesAttackError :: [ScriptError]
depositWithoutFeesAttackError = pure $
    EvaluationError
    [ "checkUserDeposit: Output value is wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"
