{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

{-|
Module      : Staking.Business.User
Description : Business logic related to the main operations that a user can
              perform.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.Business.User
  ( claim
  , compound
  , deposit
  , withdraw
  , depositFees
  , withdrawFees
  , isAfter
  , days
  , depositLevel
  , computeRewards) where

-- Third-party libraries.
import Ledger
import PlutusTx.Prelude

-- Internal modules.
import Staking.Business.Types

{-| If the amount to deposit is greater or equal than the minimum amount setted,
    then, this function computes the update of the UserState by adding a new
    element on the deposit's list and its FeesDistribution.
     * This element is a tuple with the amount given by the parameters is
       subtracted by the fees and the POSIXTime of the operation.
     * The fees are calculated using the fixed percentage set for this
       operation and the amount being deposited.

    Other way it returns Nothing.
-}
{-# INLINABLE deposit #-}
deposit
    :: UserState
    -> Integer
    -> POSIXTime
    -> OperationSettings
    -> Maybe (UserState, FeesDistribution)
deposit userState amount cTime fconf =
    if amount < minDeposit fconf
    then Nothing
    else Just (addDeposit userState cTime (amount - totalFees), fees)
  where
    fees :: FeesDistribution
    fees = depositFees amount fconf

    refFees, daoFees, affFees :: Integer
    FeesDistribution refFees daoFees affFees = fees

    totalFees :: Integer
    totalFees = refFees + daoFees + affFees

{-| If the amount to withdraw is between the minimum amount setted and the total
    amount deposited, then this function computes the update of the UserState by
    removing or updating the elements on the deposit's list and its
    FeesDistribution.
    * An element is updated when the value being withdrawn is lower than it's
      value and removed if tht's not the case.

    Other way it returns Nothing.
-}
{-# INLINABLE withdraw #-}
withdraw
    :: UserState
    -> Integer
    -> OperationSettings
    -> Maybe (UserState, FeesDistribution)
withdraw userState amount fconf =
    if amount < minWithdraw fconf || amount > totalAmount
    then Nothing
    else Just (newUserState, withdrawFees amount fconf)
  where
    totalAmount :: Integer
    totalAmount = sum $ map amountDeposit oDeposits

    newUserState :: UserState
    newUserState = userState { deposits = withdrawTx oDeposits amount }

    oDeposits :: [Deposit]
    oDeposits = deposits userState

{-| If the amount to claim is greater or equal than the minimum amount setted,
    then, this function computes the new UserState by updating his last claim
    and, the total amount of rewards to be claimed.

    Other way it returns Nothing.
-}
{-# INLINABLE claim #-}
claim
    :: UserState
    -> POSIXTime
    -> OperationSettings
    -> Maybe (UserState, Integer)
claim userState cTime fconf =
    if rewards < minClaim fconf
    then Nothing
    else Just (newUserState, rewards)
  where
    newUserState :: UserState
    newUserState = userState { lastClaim = Just cTime}

    rewards :: Integer
    rewards = computeRewards (deposits userState) (lastClaim userState) cTime

-- | This function calls claim and deposit the total amount of rewards.
{-# INLINABLE compound #-}
compound
    :: UserState
    -> POSIXTime
    -> OperationSettings
    -> Maybe (UserState, Integer)
compound userState cTime fconf =
    claim userState cTime fconf >>=
    \(userState', rews) -> Just (addDeposit userState' cTime rews, rews)

{-| This function recursively subtracts the required amount from the last
    deposit, deleting its record if its entire amount has already been
    subtracted. -}
{-# INLINABLE withdrawTx #-}
withdrawTx :: [Deposit] -> Integer -> [Deposit]
withdrawTx (lastDep : dep) amount =
    if amountDeposited > amount
    then (timeDeposit lastDep, amountDeposited - amount) : dep
    else withdrawTx dep (amount - amountDeposited)
  where
    amountDeposited :: Integer
    amountDeposited = amountDeposit lastDep

withdrawTx [] _ = []

{-# INLINABLE addDeposit #-}
addDeposit :: UserState -> POSIXTime -> Integer -> UserState
addDeposit userState cTime amount =
    userState { deposits = (cTime, amount) : deposits userState }

{- | computeFees takes:
        * the function to get the fee value (i.e., the amount charged as fees
        when a user performs either a deposit or a withdraw action),
        * the amount with respect to which the fees are calculated.
        * the settings necessary to calculate the transaction,
        * return the percentage of the fees destined to each wallet

    If there are any rounding issues, it is the incentives pool that is
    favored - that is, the first coordinate of the result.-}
{-# INLINABLE computeFees #-}
computeFees ::
    (OperationSettings -> Integer)
    -> Integer
    -> OperationSettings
    -> FeesDistribution
computeFees transactionFee amount fconf =
    FeesDistribution { refillFees = refFees
                     , daoFees = dFees
                     , affFees = aFees
                     }
  where
    refFees :: Integer
    refFees = totalFees - dFees - aFees

    dFees :: Integer
    dFees = divide (totalFees * daoShare fconf) 1_000_000

    aFees :: Integer
    aFees = divide (totalFees * affShare fconf) 1_000_000

    totalFees :: Integer
    totalFees = divide (amount * transactionFee fconf) 1_000_000

{-# INLINABLE withdrawFees #-}
withdrawFees :: Integer -> OperationSettings -> FeesDistribution
withdrawFees = computeFees withdrawFee

{-# INLINABLE depositFees #-}
depositFees :: Integer -> OperationSettings -> FeesDistribution
depositFees = computeFees depositFee

-- Check that `now` happens after all POSIXTime values in UserState.
{-# INLINABLE isAfter #-}
isAfter :: POSIXTime -> UserState -> Bool
isAfter now UserState{..} =
       all ((now >) . timeDeposit) deposits
    && case lastClaim of
            Nothing -> True
            Just lc -> now > lc


-- miliseconds per day
{-# INLINABLE msPerDay #-}
msPerDay :: Integer
msPerDay = 1000 * 60 * 60 * 24

-- miliseconds per year
{-# INLINABLE msPerYear #-}
msPerYear :: Integer
msPerYear = msPerDay * 365

-- day number to POSIXTime
{-# INLINABLE days #-}
days :: Integer -> POSIXTime
days n = POSIXTime (n * msPerDay)

{-# INLINABLE levelAPR #-}
levelAPR :: Integer -> Integer
levelAPR n | n == 1 = 150_000
           | n == 2 = 200_000

{-# INLINABLE depositLevel #-}
depositLevel :: POSIXTime -> POSIXTime -> Integer
depositLevel depositTime now =
    if (now - depositTime) < days 90
    then 1
    else 2

{-# INLINABLE rewardsPerDeposit #-}
rewardsPerDeposit
    :: Maybe POSIXTime
    -> POSIXTime
    -> Deposit
    -> Integer
rewardsPerDeposit lastClaim now (depTime, amount) =
    case lastClaim of
        Nothing -> getRewards $ getPOSIXTime (now - depTime)
        Just lClaim ->
            if lClaim < depTime
            then getRewards $ getPOSIXTime (now - depTime)
            else getRewards $ getPOSIXTime (now - lClaim)
  where
    getRewards :: Integer -> Integer
    getRewards duration = (getLevel * duration * amount) `divide` msPerYearMi

    msPerYearMi :: Integer
    msPerYearMi = msPerYear * 1_000_000

    getLevel :: Integer
    getLevel = levelAPR $ depositLevel depTime now

{-# INLINABLE computeRewards #-}
computeRewards
    :: [Deposit]
    -> Maybe POSIXTime
    -> POSIXTime
    -> Integer
computeRewards deposits lastClaim now =
    foldr getRewards 0 deposits
  where
    getRewards :: Deposit -> Integer -> Integer
    getRewards dep rews = rews + rewardsPerDeposit lastClaim now dep
