{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.OffChain.Start
Description : A test to check the behavior of the start operation.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.OffChain.Start where

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
tests = testGroup "offChainStartTests"
       [ startTest ]

startTest :: TestTree
startTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "startTest"
    (     walletFundsChange p2pWallet (mainTokenValue (-7_777_777))
     .&&. valueAtAddress (addressStaking testStaking) scriptValueOK
    )
    startTrace
  where
    scriptValueOK :: Value -> Bool
    scriptValueOK val =
        val == testStakingNFT <> mainTokenValue 7_777_777

startTrace :: EmulatorTrace ()
startTrace = do
    void $ activateContractWallet p2pWallet $ runStaking (MicroToken 7_777_777)
                                                         testStakingSettings
    void $ Emulator.waitNSlots 10
