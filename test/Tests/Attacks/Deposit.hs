{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators#-}

{-|
Module      : Tests.Attacks.Deposit
Description : A new off-chain code to test an attack the deposit validator.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.Attacks.Deposit
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
import           MainToken
import           Staking.Business
import           Staking.Types
import           Utils.OffChain
import           Tests.Attacks.AttackUtils
import           Tests.TestUtils

-- Schema.
type AttackSchema =
          Endpoint "depositNegativeAttack" MainToken
      .\/ Endpoint "depositEmptyListAttack" MainToken
      .\/ Endpoint "depositWithoutFeesAttack" MainToken

attackUserEndpoints :: Staking -> Contract (Last Staking) AttackSchema Text ()
attackUserEndpoints staking = forever
                        $ handleError logError
                        $ awaitPromise
                        $          depositNegativeEndpoint
                          `select` depositEmptyListEndpoint
                          `select` depositWithoutFeesEndpoint
  where
    depositNegativeEndpoint :: Promise (Last Staking) AttackSchema Text ()
    depositNegativeEndpoint =
        endpoint @"depositNegativeAttack" $ depositNegativeAttack staking

    depositEmptyListEndpoint :: Promise (Last Staking) AttackSchema Text ()
    depositEmptyListEndpoint =
        endpoint @"depositEmptyListAttack" $ depositEmptyListAttack staking

    depositWithoutFeesEndpoint :: Promise (Last Staking) AttackSchema Text ()
    depositWithoutFeesEndpoint =
        endpoint @"depositWithoutFeesAttack" $ depositWithoutFeesAttack staking


-- Off-chain code.
depositNegativeAttack :: forall w s. Staking -> MainToken -> Contract w s Text ()
depositNegativeAttack staking@Staking{..} am@(MicroToken amount) = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let depositRes = deposit oldUserState (getMicroToken am)
                                      cTime (opSettings settings)

    case depositRes of
        Nothing -> logInfo @String  $
                   "Intended amount is lower than minimum allowed for deposits."
        Just (newUserState,feesD) -> do
            let refFees = refillFees feesD
                dFees   = daoFees feesD
                aFees   = affFees feesD

                oldUserVal   = getChainIndexTxOutValue oUser
                totalFees    = refFees + dFees + aFees
                deposited    = getMicroToken am - totalFees
                newUserVal   = oldUserVal <> mainTokenValue deposited
                newUserDatum = UserDatum newUserState

                range        = interval cTime (cTime + validTimeRange)
                red          = Redeemer $ PlutusTx.toBuiltinData
                                        $ Deposit (MicroToken (-amount)) cTime
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

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User deposited " ++ show (MicroToken (-amount)) ++
                              " micro MyToken to their " ++
                              "script UTxO, and paid " ++ show totalFees ++
                              " micro MyToken in fees."

depositEmptyListAttack
    :: forall w s. Staking
    -> MainToken
    -> Contract w s Text ()
depositEmptyListAttack staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let depositRes = deposit oldUserState (getMicroToken am)
                                      cTime (opSettings settings)

    case depositRes of
        Nothing -> logInfo @String  $
                   "Intended amount is lower than minimum allowed for deposits."
        Just (newUserState,feesD) -> do
            let refFees = refillFees feesD
                dFees   = daoFees feesD
                aFees   = affFees feesD

                oldUserVal   = getChainIndexTxOutValue oUser
                totalFees    = refFees + dFees + aFees
                deposited    = getMicroToken am - totalFees
                newUserVal   = oldUserVal <> mainTokenValue deposited
                negativeUserState = newUserState { deposits = [] }
                newUserDatum = UserDatum negativeUserState

                range        = interval cTime (cTime + validTimeRange)
                red          = Redeemer $ PlutusTx.toBuiltinData
                                        $ Deposit am cTime
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

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User had their script UTxO deposits emptied"

depositWithoutFeesAttack
  :: forall w s. Staking
  -> MainToken
  -> Contract w s Text ()
depositWithoutFeesAttack staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let depositRes = deposit oldUserState (getMicroToken am)
                                      cTime (opSettings settings)

    case depositRes of
        Nothing -> logInfo @String  $
                   "Intended amount is lower than minimum allowed for deposits."
        Just (newUserState, _) -> do
            let oldUserVal   = getChainIndexTxOutValue oUser
                deposited    = getMicroToken am
                newUserVal   = oldUserVal <> mainTokenValue deposited
                newUserDatum = UserDatum newUserState

                range        = interval cTime (cTime + validTimeRange)
                red          = Redeemer $ PlutusTx.toBuiltinData
                                        $ Deposit am cTime
                lookups      = mkLookups staking [ (orefUser, oUser) ]
                tx =  Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User deposited " ++ show am ++ " micro " ++
                              "MyToken to their script UTxO without fees."
