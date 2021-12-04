{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators#-}

{-|
Module      : Tests.Attacks.Withdraw
Description : A new off-chain code to test an attack the withdraw validator.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.Attacks.Withdraw
    ( AttackSchema
    -- ^ Endpoints
    , attackUserEndpoints
    ) where

import           Control.Monad
import           Data.Monoid               (Last (..))
import           Data.Text                 as T (Text)

-- Third-party libraries libraries.
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import           Plutus.Contract           as Contract
import           PlutusTx

-- Internal modules.
import           Staking.Business
import           Staking.Types
import           MainToken
import           Utils.OffChain
import           Tests.Attacks.AttackUtils
import           Tests.TestUtils

-- Schema.
type AttackSchema =
          Endpoint "withdrawAttack" MainToken
      .\/ Endpoint "fakeWithdrawAttack" MainToken
      .\/ Endpoint "withdrawWithoutFeesAttack" MainToken

attackUserEndpoints :: Staking -> Contract (Last Staking) AttackSchema Text ()
attackUserEndpoints staking = forever
                        $ handleError logError
                        $ awaitPromise
                        $          withdrawAttackEndpoint
                          `select` fakeWithdrawEndpoint
                          `select` withdrawWithoutFeesEndpoint
  where
    withdrawAttackEndpoint :: Promise (Last Staking) AttackSchema Text ()
    withdrawAttackEndpoint =
        endpoint @"withdrawAttack" $ withdrawAttack staking

    fakeWithdrawEndpoint :: Promise (Last Staking) AttackSchema Text ()
    fakeWithdrawEndpoint =
        endpoint @"fakeWithdrawAttack" $ fakeWithdrawAttack staking

    withdrawWithoutFeesEndpoint :: Promise (Last Staking) AttackSchema Text ()
    withdrawWithoutFeesEndpoint =
        endpoint @"withdrawWithoutFeesAttack" $ withdrawWithoutFeesAttack staking


-- Off-chain code.
withdrawAttack :: forall w s. Staking -> MainToken -> Contract w s Text ()
withdrawAttack staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let withdrawRes = withdraw oldUserState (getMicroToken am)
                                        (opSettings settings)
    case withdrawRes of
        Nothing -> logInfo @String  $
                "Intended amount is lower than minimum allowed for withdrawals."
        Just (newUserState, feesW) -> do
            let refFees = refillFees feesW
                dFees   = daoFees feesW
                aFees   = affFees feesW

                oldUserVal   = getChainIndexTxOutValue oUser
                newUserVal   =  oldUserVal
                             <> mainTokenValue (-getMicroToken am `div` 2)
                newUserDatum = UserDatum newUserState
                range        = interval cTime (cTime + validTimeRange)
                red          = Redeemer
                                    $ PlutusTx.toBuiltinData
                                    $ Withdraw am cTime
                lookups      = mkLookups staking [ (orefUser, oUser) ]
                tx =  Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range
                   <> Constraints.mustPayToPubKey
                      (refWallet settings) (mainTokenValue refFees <> minAda 1)
                   <> Constraints.mustPayToPubKey
                      (daoWallet settings) (mainTokenValue dFees <> minAda 1)
                   <> Constraints.mustPayToPubKey
                      (affWallet settings) (mainTokenValue aFees <> minAda 1)
                   <> Constraints.mustPayToPubKey
                      ownPKH
                      (mainTokenValue (getMicroToken am `div` 2) <> minAda 1)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User withdraw " ++
                              show (getMicroToken am `div` 2) ++
                              " micro MyToken from " ++
                              "their script UTxO, and paid " ++
                              show (refFees + dFees + aFees) ++
                              " micro MyToken in fees."

fakeWithdrawAttack :: forall w s. Staking -> MainToken -> Contract w s Text ()
fakeWithdrawAttack staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let withdrawRes = withdraw oldUserState (getMicroToken am)
                                        (opSettings settings)
    case withdrawRes of
        Nothing -> logInfo @String  $
                "Intended amount is lower than minimum allowed for withdrawals."
        Just (_, feesW) -> do
            let refFees = refillFees feesW
                dFees   = daoFees feesW
                aFees   = affFees feesW

                oldUserVal   = getChainIndexTxOutValue oUser
                newUserVal   = oldUserVal <> mainTokenValue (-getMicroToken am)
                newUserDatum = UserDatum oldUserState -- State without withdraw
                range        = interval cTime (cTime + validTimeRange)
                red          = Redeemer
                                    $ PlutusTx.toBuiltinData
                                    $ Withdraw am cTime
                lookups      = mkLookups staking [ (orefUser, oUser) ]
                tx =  Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range
                   <> Constraints.mustPayToPubKey
                      (refWallet settings) (mainTokenValue refFees <> minAda 1)
                   <> Constraints.mustPayToPubKey
                      (daoWallet settings) (mainTokenValue dFees <> minAda 1)
                   <> Constraints.mustPayToPubKey
                      (affWallet settings) (mainTokenValue aFees <> minAda 1)
                   <> Constraints.mustPayToPubKey
                      ownPKH (mainTokenValue (getMicroToken am) <> minAda 1)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User withdraw " ++ show am ++
                              " micro MyToken from " ++
                              "their script UTxO, and paid " ++
                              show (refFees + dFees + aFees) ++
                              " micro MyToken in fees."

withdrawWithoutFeesAttack
  :: forall w s. Staking
  -> MainToken
  -> Contract w s Text ()
withdrawWithoutFeesAttack staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let withdrawRes = withdraw oldUserState (getMicroToken am)
                                        (opSettings settings)
    case withdrawRes of
        Nothing -> logInfo @String  $
                "Intended amount is lower than minimum allowed for withdrawals."
        Just (newUserState, _) -> do
            let oldUserVal   = getChainIndexTxOutValue oUser
                newUserVal   = oldUserVal <> mainTokenValue (-getMicroToken am)
                newUserDatum = UserDatum newUserState
                range        = interval cTime (cTime + validTimeRange)
                red          = Redeemer
                                    $ PlutusTx.toBuiltinData $ Withdraw am cTime
                lookups      = mkLookups staking [ (orefUser, oUser) ]
                tx =  Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range
                   <> Constraints.mustPayToPubKey
                        ownPKH (mainTokenValue (getMicroToken am) <> minAda 1)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User withdraw " ++ show am ++ " micro " ++
                              "MyToken to their script UTxO without fees."
