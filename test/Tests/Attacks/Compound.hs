{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Tests.Attacks.Compound
Description : A new off-chain code to test an attack the compound validator.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.Attacks.Compound
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

-- Schema.
type AttackSchema =
          Endpoint "compoundAttack" () -- Claim and steal more than excpeted
      .\/ Endpoint "fakeCompoundAttack" () -- Compound without updating lastClaim

attackUserEndpoints :: Staking -> Contract (Last Staking) AttackSchema Text ()
attackUserEndpoints staking = forever $ handleError logError $ awaitPromise $
                              compoundAttackEndpoint `select`
                              fakeCompoundAttackEndpoint
  where
    compoundAttackEndpoint :: Promise (Last Staking) AttackSchema Text ()
    compoundAttackEndpoint =
        endpoint @"compoundAttack" $ const $ compoundAttack staking

    fakeCompoundAttackEndpoint :: Promise (Last Staking) AttackSchema Text ()
    fakeCompoundAttackEndpoint =
        endpoint @"fakeCompoundAttack" $ const $ fakeCompoundAttack staking


compoundAttack :: Staking -> Contract w s Text ()
compoundAttack staking@Staking{..} = do
    cTime                   <- currentTime
    ownPKH                  <- Contract.ownPubKeyHash
    (orefStaking, oStaking) <- findStaking staking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    activeUsers             <- getPoolState oStaking
    oldUserState            <- getUserState oUser

    let compoundRes = compound oldUserState cTime (opSettings settings)

    case compoundRes of
        Nothing -> logInfo @String $
                      "Claimable rewards do not yet reach the minimum ammount."
        Just (newUserState, rews) -> do
            void $ checkMinFundsPoolUTxO staking rews
            let oldStakingVal = getChainIndexTxOutValue oStaking
                newStakingVal = oldStakingVal <> mainTokenValue (-2*rews)
                newStakingDat = PoolDatum activeUsers
                newUserDatum  =
                    UserDatum newUserState { deposits = (cTime, rews) :
                                                        deposits newUserState }

                oldUserVal    = getChainIndexTxOutValue oUser
                newUserVal    = oldUserVal <> mainTokenValue (2*rews)

                range         = interval cTime (cTime + validTimeRange)
                red           = Redeemer $ PlutusTx.toBuiltinData $
                                Compound (MicroToken { getMicroToken = 2*rews })
                                         cTime
                lookups       = mkLookups staking [ (orefStaking, oStaking)
                                                  , (orefUser, oUser)
                                                  ]

                tx =  Constraints.mustSpendScriptOutput orefStaking red
                   <> Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newStakingDat newStakingVal
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User has compounded their rewards (" ++
                              show (2*rews) ++ " micro MyToken)."

fakeCompoundAttack :: Staking -> Contract w s Text ()
fakeCompoundAttack staking@Staking{..} = do
    cTime                   <- currentTime
    ownPKH                  <- Contract.ownPubKeyHash
    (orefStaking, oStaking) <- findStaking staking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    activeUsers             <- getPoolState oStaking
    oldUserState            <- getUserState oUser

    let compoundRes = compound oldUserState cTime (opSettings settings)

    case compoundRes of
        Nothing -> logInfo @String $
                      "Claimable rewards do not yet reach the minimum ammount."
        Just (newUserState, rews) -> do
            void $ checkMinFundsPoolUTxO staking rews
            let oldStakingVal = getChainIndexTxOutValue oStaking
                newStakingVal = oldStakingVal <> mainTokenValue (-rews)
                newStakingDat = PoolDatum activeUsers
                newUserDatum  = UserDatum newUserState {lastClaim = Nothing}

                oldUserVal    = getChainIndexTxOutValue oUser
                newUserVal    = oldUserVal <> mainTokenValue rews

                range         = interval cTime (cTime + validTimeRange)
                red           = Redeemer $ PlutusTx.toBuiltinData $
                                Compound (MicroToken { getMicroToken = rews })
                                         cTime
                lookups       = mkLookups staking [ (orefStaking, oStaking)
                                              , (orefUser, oUser) ]
                tx =  Constraints.mustSpendScriptOutput orefStaking red
                   <> Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newStakingDat newStakingVal
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ "User has compounded their rewards (" ++
                              show (2*rews) ++ " micro MyToken)."
