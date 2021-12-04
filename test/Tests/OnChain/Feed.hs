{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OnChain.Feed
Description : A test to simulate an attach on the on-chain code of feed.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OnChain.Feed where

-- GHC libraries.
import Control.Monad

-- Third-praty libraries.
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Scripts
import Test.Tasty

-- Internal modules.
import MainToken
import Staking

import Tests.Attacks.Feed
import Tests.TestUtils

tests :: TestTree
tests = testGroup "onChainFeedTests"
        [ buildTest
            "feedAttackTest"
            feedAttackError
            feedAttackTrace
        ]

feedAttackTrace :: EmulatorTrace ()
feedAttackTrace = do
    hAdminWallet <- activateContractWallet adminWallet $ runStaking
                                                         (MicroToken 9_999_999)
                                                         testStakingSettings
    void $ Emulator.waitNSlots 2
    staking <- getStaking hAdminWallet

    hAttack <- activateContractWallet adminWallet $ attackStakingEndpoints staking

    callEndpoint @"feedNegativeAttack" hAttack (MicroToken 2_222_222)
    void $ Emulator.waitNSlots 1

feedAttackError :: [ScriptError]
feedAttackError = pure $
    EvaluationError
    [ "checkPoolFeed: Output value is wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"
