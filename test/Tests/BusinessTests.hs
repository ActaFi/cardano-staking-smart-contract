{-# LANGUAGE NumericUnderscores #-}

{-|
Module      : Tests.BusinessTests
Description : A list of tests verify the behavior of business logic operations.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.BusinessTests ( tests ) where

-- Third-party libraries.
import qualified Ledger (POSIXTime)
import Test.Tasty
import Test.Tasty.HUnit

-- Internal modules.
import Staking.Business.User
import Staking.Business.Types
import Tests.TestUtils

tests :: TestTree
tests = testGroup
            "businessTests"
            [ computeFeesTest
            , computeRewardsTests
            , computeDepositLevelTests
            , businessDepositTest
            , businessWithdrawTest
            , businessClaimTest
            , businessCompoundTest
            ]

computeFeesTest :: TestTree
computeFeesTest = testGroup "computeFeesTest"
        [ testCase "Testing the computation of deposit fees"
          (depositFees 33_333_333 testOperationSettings @?= outputDepositFees)
        , testCase "Testing the computation of withdraw fees"
          (withdrawFees 29_999_999 testOperationSettings @?= outputWithdrawFees)
        ]
  where
    -- rounding is in favour of the incentives pool
    outputDepositFees, outputWithdrawFees :: FeesDistribution
    outputDepositFees  = FeesDistribution 125_001 49_999 74_999
    outputWithdrawFees = FeesDistribution 75_001 29_999 44_999

computeRewardsTests :: TestTree
computeRewardsTests = testGroup "computeRewards"
        [ testCase "basic" $
            computeRewards [(0, 100_000_000)] (Just 0) (days 10) @?= 410_958

        , testCase "noDeposits" $
            computeRewards [] (Just 0) (days 1) @?= 0

        , testCase "twoDeposits" $
            let txs       = [ (days  0, 100_000_000)
                            , (days 95, 100_000_000)
                            ]
                lClaim    = Just 0
                now       = days 120
                reward    =   6_575_342 -- 120 days, 100 P2P, level 2
                            + 1_027_397 --  25 days, 100 P2P, level 1
            in
                computeRewards txs lClaim now @?= reward

        , testCase "lastClaim" $
            let txs       = [(1596059100999 :: Ledger.POSIXTime, 32916667)]
                -- 2s before deposit
                lClaim    = Just (1596059098999 :: Ledger.POSIXTime)
                -- 100s after deposit
                now       = 1596059200999 :: Ledger.POSIXTime
                reward    = 15
            in
                computeRewards txs lClaim now @?= reward
        ]

computeDepositLevelTests :: TestTree
computeDepositLevelTests = testGroup "getLevel"
        [ testCase "duration000"       $ depositLevel         0         0  @?= 1
        , testCase "duration089"       $ depositLevel         0 (days  89) @?= 1
        , testCase "duration090"       $ depositLevel         0 (days  90) @?= 2
        , testCase "duration091"       $ depositLevel         0 (days  91) @?= 2
        , testCase "duration180"       $ depositLevel         0 (days 180) @?= 2
        , testCase "dep001duration090" $ depositLevel (days  1) (days  91) @?= 2
        , testCase "dep020duration089" $ depositLevel (days 20) (days 109) @?= 1
        , testCase "dep090duration010" $ depositLevel (days 90) (days 100) @?= 1
        , testCase "dep050duration091" $ depositLevel (days 50) (days 141) @?= 2
        ]


businessDepositTest :: TestTree
businessDepositTest = testGroup "businessDepositTest"
        [ testCase
          "Testing a single time deposit with an amount lower than minDeposit"
          (deposit uStateTest 999 (days 0) testOperationSettings @?= Nothing)

        , testCase
          "Testing a single time deposit with the same amount of minDeposit" $
          let
            uStateOutput = uStateTest { deposits = [(days 0, outputValue)] }
            outputValue  = 1_000_000 - (3_750 + 1_500 + 2_250)
            value        = 1_000_000
            fees         = FeesDistribution 3_750 1_500 2_250
            result       = Just (uStateOutput, fees)
          in
            deposit uStateTest value (days 0) testOperationSettings @?= result

        , testCase
          "Testing a single time deposit with amount greater than minDeposit" $
          let
            uStateOutput = uStateTest { deposits = [(days 0, outputValue)] }
            outputValue  = 10_000_000 - (37_500 + 15_000 + 22_500)
            value        = 10_000_000
            fees         = FeesDistribution 37_500 15_000 22_500
            result       = Just (uStateOutput, fees)
          in
            deposit uStateTest value (days 0) testOperationSettings @?= result

        , testCase
          "Testing a second deposit with an amount greater than minDeposit" $
          let
            uState       = uStateTest { deposits = [(days 0, 10_000_000)] }
            uStateOutput = uStateTest { deposits = [(days 1, outputValue),
                                                    (days 0, 10_000_000)] }
            outputValue  = 20_000_000 - (75_000 + 30_000 + 45_000)
            value        = 20_000_000
            fees         = FeesDistribution 75_000 30_000 45_000
            result       = Just (uStateOutput, fees)
          in
            deposit uState value (days 1) testOperationSettings @?= result
        ]
  where
    uStateTest :: UserState
    uStateTest = mkUserState user1WalletPKH [] Nothing

businessWithdrawTest :: TestTree
businessWithdrawTest = testGroup "businessWithdrawTest"
        [ testCase
          "Testing withdraw from an empty list of deposits" $
          let
            uStateEmptyTest = mkUserState user1WalletPKH [] Nothing
            value           = 2_000_000
          in
            withdraw uStateEmptyTest value testOperationSettings @?= Nothing

        , testCase
          "Testing withdraw an amount lower than the minWithdraw"
          (withdraw uStateTest 999 testOperationSettings @?= Nothing)

        , testCase
          "Testing withdraw the same amount of minWithdraw" $
          let
            uStateOutput = uStateTest { deposits = [(days 0, 99_000_000)] }
            value        = 1_000_000
            fees         = FeesDistribution 2_500 1_000 1_500
            result       = Just (uStateOutput, fees)
          in
            withdraw uStateTest value testOperationSettings @?= result

        , testCase
          "Testing withdraw the same amount of minWithdraw on multiple deposits" $
          let
            uState           = uStateTest { deposits = [(days 2, 200_000),
                                                        (days 1, 300_000),
                                                        (days 0, 1_000_000)] }
            uStateMultOutput = uStateTest { deposits = [(days 0, 500_000)] }
            value            = 1_000_000
            fees             = FeesDistribution 2_500 1_000 1_500
            result           = Just (uStateMultOutput, fees)
          in
            withdraw uState value testOperationSettings @?= result

        , testCase
          "Testing withdraw an amount greater than the minWithdraw" $
          let
            uStateOutput = uStateTest { deposits = [(days 0, 98_000_000)] }
            value        = 2_000_000
            fees         = FeesDistribution 5_000 2_000 3_000
            result       = Just (uStateOutput, fees)
          in
            withdraw uStateTest value testOperationSettings @?= result

        , testCase
          ("Testing withdraw an amount greater than minWithdraw on multiple " ++
          "deposits") $
          let
            uState       = uStateTest { deposits = [(days 2, 5_000_000),
                                                    (days 1, 20_000_000),
                                                    (days 0, 75_000_000)] }
            uStateOutput = uStateTest { deposits = [(days 0, 70_000_000)] }
            value        = 30_000_000
            fees         = FeesDistribution 75_000 30_000 45_000
            result       = Just (uStateOutput, fees)
          in
            withdraw uState value testOperationSettings @?= result

        , testCase
          "Testing withdraw a valid amount lower than the totalAmount" $
          let
            uStateOutput = uStateTest { deposits = [(days 0, 1_000_000)] }
            value        = 99_000_000
            fees         = FeesDistribution 247_500 99_000 148_500
            result       = Just (uStateOutput, fees)
          in
            withdraw uStateTest value testOperationSettings @?= result

        , testCase
          "Testing withdraw the same amount of totalAmount" $
          let
            uStateOutput = uStateTest { deposits = [] }
            value        = 100_000_000
            fees         = FeesDistribution 250_000 100_000 150_000
            result       = Just (uStateOutput, fees)
          in
            withdraw uStateTest value testOperationSettings @?= result

        , testCase
          "Testing withdraw an amount greater than totalAmount"
          (withdraw uStateTest 100_000_001 testOperationSettings @?= Nothing)

        , testCase
          ("Testing withdraw an amount greater than totalAmount on multiple" ++
          "deposits")
          (withdraw uStateTest 100_000_001 testOperationSettings @?= Nothing)
        ]
  where
    uStateTest :: UserState
    uStateTest = mkUserState user1WalletPKH [(days 0, 100_000_000)] Nothing

businessClaimTest :: TestTree
businessClaimTest = testGroup "businessClaimTest"
        [ testCase
          "Testing claim rewards from an empty list of deposits" $
          let
            uStateEmptyTest = mkUserState user1WalletPKH [] Nothing
            now             = days 1
          in
            claim uStateEmptyTest now testOperationSettings @?= Nothing

        , testCase
          "Testing claim a reward lower than the minClaim" $
          let
            uState = uStateTest { deposits = [(days 0, 100_000_000)] }
            now    = 21023 :: Ledger.POSIXTime
          in
          claim uState now testOperationSettings @?= Nothing

        , testCase
          "Testing claim the same amount of minClaim with level 1" $
          let
            now          = 4204800000 :: Ledger.POSIXTime
            rewards      = 10_000_000
            result       = Just (uStateOutputTest now, rewards)
          in
            claim uStateTest now testOperationSettings @?= result

        , testCase
          "Testing claim the same amount of minClaim with level 2" $
          let
            uState       = uStateTest { deposits = [(days 0, 202_777_779)] }
            uStateOutput = uState     { lastClaim = Just now }
            now          = days 90
            rewards      = 10_000_000
            result       = Just (uStateOutput, rewards)
          in
            claim uState now testOperationSettings @?= result

      , testCase
        "Testing claim a reward greater than minClaim with level 1 and 89 days" $
        let
          now     = days 89
          rewards = 18_287_671
          result  = Just (uStateOutputTest now, rewards)
        in
          claim uStateTest now testOperationSettings @?= result

      , testCase
        "Testing claim a reward greater than minClaim with level 2 and 90 days" $
        let
          now     = days 90
          rewards = 24_657_534
          result  = Just (uStateOutputTest now, rewards)
        in
          claim uStateTest now testOperationSettings @?= result

      , testCase
        "Testing claim a reward greater than minClaim with level 2 and 91 days" $
        let
          now     = days 91
          rewards = 24_931_506
          result  = Just (uStateOutputTest now, rewards)
        in
          claim uStateTest now testOperationSettings @?= result
        ]
  where
    uStateTest :: UserState
    uStateTest = mkUserState user1WalletPKH [(days 0, 500_000_000)] Nothing

    uStateOutputTest :: Ledger.POSIXTime -> UserState
    uStateOutputTest now = uStateTest { lastClaim = Just now }

businessCompoundTest :: TestTree
businessCompoundTest = testGroup "businessCompoundTest"
        [ testCase
          "Testing compound rewards from an empty list of deposits" $
          let
            uStateEmptyTest = mkUserState user1WalletPKH [] Nothing
            now             = days 1
          in
            compound uStateEmptyTest now testOperationSettings @?= Nothing

        , testCase
          "Testing compound a reward lower than the minClaim" $
          let
            uState = uStateTest { deposits = [(days 0, 100_000_000)] }
            now    = 21023 :: Ledger.POSIXTime
          in
            compound uState now testOperationSettings @?= Nothing

        , testCase
          "Testing compound the same amount of minClaim with level 1" $
          let
            now          = 4204800000 :: Ledger.POSIXTime
            rewards      = 10_000_000
            result       = uStateOutputTest now rewards
          in
            compound uStateTest now testOperationSettings @?= result

        , testCase
          "Testing compound the same amount of minClaim with level 2" $
          let
            uState       = uStateTest { deposits  = [(days 0, 202_777_779)] }
            uStateOutput = uState     { deposits  =
                                        (now, rewards) : deposits uState
                                      , lastClaim = Just now }
            now          = days 90
            rewards      = 10_000_000
            result       = Just (uStateOutput, rewards)
          in
            compound uState now testOperationSettings @?= result

        , testCase
          ("Testing compound a reward greater than minClaim with level 2 and " ++
          "89 days") $
          let
            now     = days 89
            rewards = 18_287_671
            result  = uStateOutputTest now rewards
          in
            compound uStateTest now testOperationSettings @?= result

        , testCase
          ("Testing compound a reward greater than minClaim with level 2 and " ++
          "90 days") $
          let
            now     = days 90
            rewards = 24_657_534
            result  = uStateOutputTest now rewards
          in
            compound uStateTest now testOperationSettings @?= result

        ,  testCase
          ("Testing compound a reward greater than minClaim with level 2 and " ++
          "91 days") $
          let
            now     = days 91
            rewards = 24_931_506
            result  = uStateOutputTest now rewards
          in
            compound uStateTest now testOperationSettings @?= result
        ]
  where
    uStateTest :: UserState
    uStateTest = mkUserState user1WalletPKH [(days 0, 500_000_000)] Nothing

    uStateOutputTest :: Ledger.POSIXTime -> Integer ->  Maybe (UserState,Integer)
    uStateOutputTest now rews = let
      uStateOutput = uStateTest { deposits = (now, rews) : deposits uStateTest
                                , lastClaim = Just now }
      in Just (uStateOutput, rews)
