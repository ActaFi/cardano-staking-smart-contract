{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Tests.Attacks.Claim
Description : A new off-chain code to test an attack the claim validator.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.Attacks.Claim
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
          Endpoint "claimAttack" () -- Claim and steal more than excepted

attackUserEndpoints :: Staking -> Contract (Last Staking) AttackSchema Text ()
attackUserEndpoints staking = forever $ handleError logError $ awaitPromise
                              claimAttackEndpoint
  where
    claimAttackEndpoint :: Promise (Last Staking) AttackSchema Text ()
    claimAttackEndpoint =
        endpoint @"claimAttack" $ const $ claimAttack staking

{-| Attack Summary:
    This attack attempts to claim twice the deserved reward for a claim
    operation.

    Modifications from the original OffChain code:
     * The newStakingValue consists of the old subtracted by twice the rewards
       of the claim operation.
     * Pay to the user's wallet twice the rewards of the claim operation.
-}

claimAttack :: forall w s. Staking -> Contract w s Text ()
claimAttack staking@Staking{..} = do
    cTime                   <- currentTime
    ownPKH                  <- Contract.ownPubKeyHash
    (orefStaking, oStaking) <- findStaking staking
    activeUsers             <- getPoolState oStaking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    oldUserState            <- getUserState oUser

    let claimRes = claim oldUserState cTime (opSettings settings)

    case claimRes of
        Nothing -> logInfo @String $
                      "Claimable rewards do not yet reach the minimum ammount."
        Just (newUserState, rews) -> do
            void $ checkMinFundsPoolUTxO staking rews
            let oldStakingVal = getChainIndexTxOutValue oStaking
                newStakingVal = oldStakingVal <> mainTokenValue (-2*rews)
                newStakingDat = PoolDatum activeUsers
                newUserDatum  = UserDatum newUserState
                newUserVal    = getChainIndexTxOutValue oUser

                range         = interval cTime (cTime + validTimeRange)
                red           = Redeemer $ PlutusTx.toBuiltinData $
                                Claim (MicroToken { getMicroToken = rews }) cTime
                lookups       = mkLookups staking [ (orefStaking, oStaking)
                                              , (orefUser, oUser) ]
                tx =  Constraints.mustSpendScriptOutput orefStaking red
                   <> Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newStakingDat newStakingVal
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range
                   <> Constraints.mustPayToPubKey
                        ownPKH (mainTokenValue (2*rews) <> minAda 1)

            logInfo @String $
               "Trying to claim rewards of (" ++ show (2*rews) ++
               " micro tokens)."
            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
