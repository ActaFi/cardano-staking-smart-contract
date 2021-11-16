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
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 as T ( Text, pack
                                                )

-- Third-party libraries libraries.
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import           Ledger.Value              as Value
import           Plutus.Contract           as Contract
import           PlutusTx

-- Internal modules.
import           Staking
import           MainToken
import           Utils.OffChain

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


compoundAttack :: forall w s. Staking -> Contract w s Text ()
compoundAttack staking@Staking{..} = do
    cTime                   <- currentTime
    ownPKH                  <- pubKeyHash <$> Contract.ownPubKey
    (orefStaking, oStaking) <- findStaking staking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    activeUsers             <- getActiveUsers oStaking
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
            void $ awaitTxConfirmed $ txId submittedTx
            logInfo @String $ "User has compounded their rewards (" ++
                              show (2*rews) ++ " micro MyToken)."

fakeCompoundAttack :: forall w s. Staking -> Contract w s Text ()
fakeCompoundAttack staking@Staking{..} = do
    cTime                   <- currentTime
    ownPKH                  <- pubKeyHash <$> Contract.ownPubKey
    (orefStaking, oStaking) <- findStaking staking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    activeUsers             <- getActiveUsers oStaking
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
            void $ awaitTxConfirmed $ txId submittedTx
            logInfo @String $ "User has compounded their rewards (" ++
                              show (2*rews) ++ " micro MyToken)."

-- Helper functions.
mkLookups ::
       Staking
    -> [(TxOutRef, ChainIndexTxOut)]
    -> ScriptLookups StakingType
mkLookups p utxos =
       Constraints.typedValidatorLookups (typedValidatorStaking p)
    <> Constraints.otherScript (validatorStaking p)
    <> Constraints.unspentOutputs (Map.fromList utxos)

-- | Monadic function for getting the datum from a staking pool UTxO.

-- MANU NOTE: Change function name to getPoolState or something like that.
getActiveUsers :: ChainIndexTxOut -> Contract w s T.Text PoolState
getActiveUsers o =
    case getChainIndexTxOutDatum o of
        Just (PoolDatum ps) -> return ps
        Just (UserDatum _)  ->
            throwError "Expected StakingDatum but found UserDatum."
        Nothing             -> throwError "Cannot find contract datum."

-- | Monadic function for getting the UserState from a user script UTxO.
getUserState :: forall w s. ChainIndexTxOut -> Contract w s T.Text UserState
getUserState o = case getChainIndexTxOutDatum o of
    Just dat -> case dat of
        PoolDatum _   ->
            throwError "Expected UserDatum but found StakingDatum."
        UserDatum res -> return res
    Nothing  -> throwError "Cannot find contract datum."

{- | Monadic function returning the user script UTxO corresponding to the
     PubKeyHash. -}
findUserUTxO :: forall w s.
       Staking
    -> PubKeyHash
    -> Contract w s Text (TxOutRef, ChainIndexTxOut)
findUserUTxO staking pkh = do
    (_, oStaking) <- findStaking staking
    dat           <- getContractDatum oStaking
    case dat of
        UserDatum _             -> throwError
            "Expected StakingDatum but found UserDatum in staking script UTxO."
        PoolDatum ps ->
            case getUserNFT ps pkh of
                Just userNFT -> lookupScriptUTxO
                                    (addressStaking staking)
                                    userNFT
                _            -> throwError "Could not find user NFT."

-- | Monadic function for getting the datum from a ChainIndexTxOut.
getContractDatum :: ChainIndexTxOut -> Contract w s T.Text StakingDatum
getContractDatum =
    maybe (throwError "Cannot find contract datum") return .
          getChainIndexTxOutDatum

{- | Monadic function for checking if there is enough funds in pool UTxO for a
     claim or compound transaction -}
checkMinFundsPoolUTxO :: forall w s. Staking -> Integer -> Contract w s T.Text ()
checkMinFundsPoolUTxO staking rews = do
    (_, oStaking) <- findStaking staking
    when
      (assetClassValueOf (getChainIndexTxOutValue oStaking) mainTokenAC < rews)
      $ throwError $ pack $ "Claim or Compound transaction not issued due to "
                          ++ "unsufficient funds in pool UTxO."
