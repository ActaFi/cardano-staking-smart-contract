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
    ownPKH            <- pubKeyHash <$> Contract.ownPubKey
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
                                (refWallet settings) (mainTokenValue refFees)
                   <> Constraints.mustPayToPubKey
                                (daoWallet settings) (mainTokenValue dFees)
                   <> Constraints.mustPayToPubKey
                                (affWallet settings) (mainTokenValue aFees)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId submittedTx
            logInfo @String $ "User withdraw " ++
                              show (getMicroToken am `div` 2) ++
                              " micro MyToken from " ++
                              "their script UTxO, and paid " ++
                              show (refFees + dFees + aFees) ++
                              " micro MyToken in fees."

fakeWithdrawAttack :: forall w s. Staking -> MainToken -> Contract w s Text ()
fakeWithdrawAttack staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- pubKeyHash <$> Contract.ownPubKey
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
                                (refWallet settings) (mainTokenValue refFees)
                   <> Constraints.mustPayToPubKey
                                (daoWallet settings) (mainTokenValue dFees)
                   <> Constraints.mustPayToPubKey
                                (affWallet settings) (mainTokenValue aFees)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId submittedTx
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
    ownPKH            <- pubKeyHash <$> Contract.ownPubKey
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

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ txId submittedTx
            logInfo @String $ "User withdraw " ++ show am ++ " micro " ++
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
