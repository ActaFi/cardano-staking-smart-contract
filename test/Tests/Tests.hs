{-|
Module      : Tests.Tests
Description : A module to export all tests to the the main test file.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.Tests ( tests ) where

-- Third-party libraries.
import           Test.Tasty

-- Internal modules.
import qualified Tests.BusinessTests
import qualified Tests.OffChain.Claim
import qualified Tests.OffChain.Compound
import qualified Tests.OffChain.Deposit
import qualified Tests.OffChain.Feed
import qualified Tests.OffChain.Register
import qualified Tests.OffChain.Start
import qualified Tests.OffChain.Unregister
import qualified Tests.OffChain.Withdraw
--import qualified Tests.OffChain.UseCaseTests
import qualified Tests.OnChain.Claim
import qualified Tests.OnChain.Compound
import qualified Tests.OnChain.Deposit
import qualified Tests.OnChain.Feed
import qualified Tests.OnChain.Withdraw

tests :: TestTree
tests = testGroup "all"
        [ Tests.BusinessTests.tests
        , Tests.OffChain.Claim.tests
        , Tests.OffChain.Compound.tests
        , Tests.OffChain.Deposit.tests
        , Tests.OffChain.Feed.tests
        , Tests.OffChain.Register.tests
        , Tests.OffChain.Start.tests
        , Tests.OffChain.Unregister.tests
        , Tests.OffChain.Withdraw.tests
      --  , Tests.OffChain.UseCaseTests.tests
        , Tests.OnChain.Claim.tests
        , Tests.OnChain.Compound.tests
        , Tests.OnChain.Deposit.tests
        , Tests.OnChain.Feed.tests
        , Tests.OnChain.Withdraw.tests
        ]
