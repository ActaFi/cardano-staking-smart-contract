{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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
import           Staking.Types
import           Staking.Validator
import           MainToken
import           Utils.OffChain

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

feedNegativeAttack :: forall w s. Staking -> MainToken -> Contract w s Text ()
feedNegativeAttack staking am = do
        (orefStaking, oStaking) <- findStaking staking
        stakingDat              <- getContractDatum oStaking

        let newDat  = stakingDat
            oldVal  = getChainIndexTxOutValue oStaking
            newVal  = oldVal <> mainTokenValue (getMicroToken am)

            red     = Redeemer $ PlutusTx.toBuiltinData $ Feed am
            lookups = mkLookups staking [(orefStaking, oStaking)]
            tx      =
                   Constraints.mustSpendScriptOutput orefStaking red
                <> Constraints.mustPayToTheScript newDat newVal

        submittedTx <- submitTxConstraintsWith lookups tx
        void $ awaitTxConfirmed $ txId submittedTx
        logInfo @String $ "Staking fed with " ++ show am ++
                          " micro MyToken tokens."

-- Helper functions.
mkLookups ::
       Staking
    -> [(TxOutRef, ChainIndexTxOut)]
    -> ScriptLookups StakingType
mkLookups p utxos =
       Constraints.typedValidatorLookups (typedValidatorStaking p)
    <> Constraints.otherScript (validatorStaking p)
    <> Constraints.unspentOutputs (Map.fromList utxos)

-- | Monadic function returning the UTxO corresponding to the staking pool.
findStaking :: forall w s.
       Staking
    -> Contract w s Text (TxOutRef, ChainIndexTxOut)
findStaking staking@Staking{..} = lookupScriptUTxO (addressStaking staking) nft

-- | Monadic function for getting the datum from a ChainIndexTxOut.
getContractDatum :: ChainIndexTxOut -> Contract w s T.Text StakingDatum
getContractDatum =
    maybe (throwError "Cannot find contract datum") return .
          getChainIndexTxOutDatum
