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
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 as T ( Text
                                                )

-- Third-party libraries libraries.
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import           Plutus.Contract           as Contract
import           PlutusTx

-- Internal modules.
import           Staking
import           MainToken
import           Utils.OffChain

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
    ownPKH            <- pubKeyHash <$> Contract.ownPubKey
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
                                (refWallet settings) (mainTokenValue refFees)
                   <> Constraints.mustPayToPubKey
                                (daoWallet settings) (mainTokenValue dFees)
                   <> Constraints.mustPayToPubKey
                                (affWallet settings) (mainTokenValue aFees)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId submittedTx
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
    ownPKH            <- pubKeyHash <$> Contract.ownPubKey
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
                                (refWallet settings) (mainTokenValue refFees)
                   <> Constraints.mustPayToPubKey
                                (daoWallet settings) (mainTokenValue dFees)
                   <> Constraints.mustPayToPubKey
                                (affWallet settings) (mainTokenValue aFees)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId submittedTx
            logInfo @String $ "User had their script UTxO deposits emptied"

depositWithoutFeesAttack
  :: forall w s. Staking
  -> MainToken
  -> Contract w s Text ()
depositWithoutFeesAttack staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- pubKeyHash <$> Contract.ownPubKey
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
            void $ awaitTxConfirmed $ txId submittedTx
            logInfo @String $ "User deposited " ++ show am ++ " micro " ++
                              "MyToken to their script UTxO without fees."

-- Helper functions.
mkLookups ::
       Staking
    -> [(TxOutRef, ChainIndexTxOut)]
    -> ScriptLookups StakingType
mkLookups p utxos =
       Constraints.typedValidatorLookups (typedValidatorStaking p)
    <> Constraints.otherScript (validatorStaking p)
    <> Constraints.unspentOutputs (Map.fromList utxos)

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
