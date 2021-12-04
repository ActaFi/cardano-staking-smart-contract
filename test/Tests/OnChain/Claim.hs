{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OnChain.Claim
Description : A test to simulate an attach on the on-chain code of claim.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OnChain.Claim where

-- GHC libraries.
import Control.Monad

-- Third-praty libraries.
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Scripts
import Test.Tasty

-- Internal modules.
import MainToken
import Staking

import Tests.Attacks.Claim
import Tests.TestUtils

tests :: TestTree
tests = testGroup "onChainClaimTests"
        [ buildTest
            "claimAttackTest"
            claimAttackError
            claimAttackTrace
        ]

claimAttackTrace :: EmulatorTrace ()
claimAttackTrace = do
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
    void $ waitNSlots 100

    callEndpoint @"claimAttack" hAttack ()
    void $ Emulator.waitNSlots 2

claimAttackError :: [ScriptError]
claimAttackError = pure $
    EvaluationError
    [ "checkPoolClaim: Pool ouput UTxO value is wrong."
    , "PT5"
    ]
    "CekEvaluationFailure"
