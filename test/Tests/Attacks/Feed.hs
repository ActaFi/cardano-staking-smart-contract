{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Tests.Attacks.Feed
Description : A new off-chain code to test an attack the feed on-chain validator.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Tests.Attacks.Feed
    ( AttackSchema
    -- ^ Endpoints
    , attackStakingEndpoints
    ) where

-- GHC libraries.
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
import           Staking.Types
import           Utils.OffChain
import           Tests.Attacks.AttackUtils
import           Tests.TestUtils

-- Schema.
type AttackSchema = Endpoint "feedNegativeAttack" MainToken

attackStakingEndpoints :: Staking -> Contract (Last Staking) AttackSchema Text ()
attackStakingEndpoints staking = forever
                        $ handleError logError
                        $ awaitPromise feedNegativeEndpoint
  where
    feedNegativeEndpoint :: Promise (Last Staking) AttackSchema Text ()
    feedNegativeEndpoint = endpoint @"feedNegativeAttack" $
                                      feedNegativeAttack staking

feedNegativeAttack :: Staking -> MainToken -> Contract w s Text ()
feedNegativeAttack staking am = do
        (orefStaking, oStaking) <- findStaking staking
        ownPKH                  <- Contract.ownPubKeyHash
        stakingDat              <- getContractDatum oStaking

        let newDat     = stakingDat
            oldVal     = getChainIndexTxOutValue oStaking
            negativeAm = mainTokenValue (-(getMicroToken am))
            positiveAm = mainTokenValue (getMicroToken am)
            newVal     = oldVal <> negativeAm

            red     = Redeemer $ PlutusTx.toBuiltinData $ Feed am
            lookups = mkLookups staking [(orefStaking, oStaking)]
            tx      =
                   Constraints.mustSpendScriptOutput orefStaking red
                <> Constraints.mustPayToTheScript newDat newVal
                <> Constraints.mustPayToPubKey ownPKH (positiveAm <> minAda 1)

        submittedTx <- submitTxConstraintsWith lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId submittedTx
        logInfo @String $ "Staking fed with " ++ show am ++
                          " micro MyToken tokens."
