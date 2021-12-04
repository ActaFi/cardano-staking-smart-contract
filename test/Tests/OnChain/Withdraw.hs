{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OnChain.Withdraw
Description : A test to simulate an attach on the on-chain code of withdraw.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OnChain.Withdraw where

-- GHC libraries.
import Control.Monad

-- Third-praty libraries.
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Scripts
import Test.Tasty

-- Internal modules.
import MainToken
import Staking

import Tests.Attacks.Withdraw
import Tests.TestUtils

tests :: TestTree
tests = testGroup "onChainWithdrawTests"
        [ buildTest
            "withdrawAttackTest"
            withdrawAttackError
            withdrawAttackTrace
        , buildTest
            "fakeWithdrawAttackTest"
            fakeWithdrawAttackError
            fakeWithdrawAttackTrace
        , buildTest
            "withdrawWithoutFeesAttackTest"
            withdrawWithoutFeesAttackError
            withdrawWithoutFeesAttackTrace
        ]

withdrawAttackTrace :: EmulatorTrace ()
withdrawAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                     (MicroToken 9_999_999)
                                                     testStakingSettings
    pool <- getStaking hAdminWallet
    void $ Emulator.waitNSlots 2

    hUser     <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack   <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hUser ()
    void $ waitNSlots 2

    callEndpoint @"deposit" hUser (MicroToken 33_333_333)
    void $ waitNSlots 2

    -- We expect that this just half of this value will be withdrawn
    callEndpoint @"withdrawAttack" hAttack (MicroToken 2_222_222)
    void $ Emulator.waitNSlots 10

withdrawAttackError :: [ScriptError]
withdrawAttackError = pure $
    EvaluationError
    [ "checkUserWithdraw: Output value is wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"

fakeWithdrawAttackTrace :: EmulatorTrace ()
fakeWithdrawAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                     (MicroToken 9_999_999)
                                                     testStakingSettings
    pool <- getStaking hAdminWallet
    void $ Emulator.waitNSlots 2

    hUser     <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack   <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hUser ()
    void $ waitNSlots 2

    callEndpoint @"deposit" hUser (MicroToken 33_333_333)
    void $ waitNSlots 2

    -- We expect that this value never change the amount of tokens in the wallet
    callEndpoint @"fakeWithdrawAttack" hAttack (MicroToken 2_222_222)
    void $ Emulator.waitNSlots 10

fakeWithdrawAttackError :: [ScriptError]
fakeWithdrawAttackError = pure $
    EvaluationError
    [ "checkUserWithdraw: Datum is wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"

withdrawWithoutFeesAttackTrace :: EmulatorTrace ()
withdrawWithoutFeesAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                     (MicroToken 9_999_999)
                                                     testStakingSettings
    pool <- getStaking hAdminWallet
    void $ Emulator.waitNSlots 2

    hUser     <- activateContractWallet user1Wallet $ userEndpoints pool
    hAttack   <- activateContractWallet user1Wallet $ attackUserEndpoints pool

    void $ waitNSlots 2

    callEndpoint @"register" hUser ()
    void $ waitNSlots 2

    callEndpoint @"deposit" hUser (MicroToken 33_333_333)
    void $ waitNSlots 2

    callEndpoint @"withdrawWithoutFeesAttack" hAttack (MicroToken 2_222_222)
    void $ Emulator.waitNSlots 10

withdrawWithoutFeesAttackError :: [ScriptError]
withdrawWithoutFeesAttackError = pure $
    EvaluationError
    [ "checkFeesDistribution: Incentives pool fees are wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"
