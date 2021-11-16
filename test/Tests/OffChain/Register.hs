{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Register
Description : A test to check the behavior of the register operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Register where

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
import Staking.OffChain
import Staking.Validator

import Tests.TestUtils

tests :: TestTree
tests = testGroup "offChainRegisterTests"
       [ registerTest ]

registerTest :: TestTree
registerTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "registerTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    registerTrace
  where
    scriptValueOK :: Value -> Bool
    scriptValueOK val = val ==    testStakingNFT
                               <> testUserNFT
                               <> mainTokenValue 7_777_777

registerTrace  :: EmulatorTrace ()
registerTrace = do
    hp2pWallet <- activateContractWallet p2pWallet $ runStaking
                                                     (MicroToken 7_777_777)
                                                     testStakingSettings
    pool       <- getStaking hp2pWallet
    void $ waitNSlots 2
    hUser1     <- activateContractWallet user1Wallet $ userEndpoints pool
    void $ waitNSlots 2
    callEndpoint @"register" hUser1 ()
    void $ waitNSlots 10
