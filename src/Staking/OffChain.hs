{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Staking.OffChain
Description : Off-chain code of the staking pool.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.OffChain
    ( StakingSchema
    , runStaking
    -- ^ Endpoints
    , stakingEndpoints
    , userEndpoints
    -- ^ Utils
    , findStaking
    ) where

-- GHC libraries.
import           Control.Monad
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 as T (Text, pack)

-- Third-party libraries libraries.
import           Ledger                    hiding (singleton)
import qualified Ledger.Ada                       as Ada
import           Ledger.Constraints        as Constraints
import           Ledger.Value              as Value
import           Plutus.Contract           as Contract
import           Plutus.Contracts.Currency as Currency

-- Internal modules.
import qualified Staking.Business          as Business
import           Staking.Types
import           Staking.Validator
import           Staking.Tokens
import           MainToken
import           Utils.OffChain

-- Schema.
type StakingSchema = Endpoint "feed"       MainToken
                 .\/ Endpoint "register"   ()
                 .\/ Endpoint "unregister" ()
                 .\/ Endpoint "deposit"    MainToken
                 .\/ Endpoint "withdraw"   MainToken
                 .\/ Endpoint "claim"      ()
                 .\/ Endpoint "compound"   ()

runStaking
    :: MainToken
    -> StakingSettings
    -> Contract (Last Staking) StakingSchema Text ()
runStaking am sett = do
    staking <- start am sett
    tell $ Last $ Just staking
    stakingEndpoints staking

stakingEndpoints :: Staking -> Contract (Last Staking) StakingSchema Text ()
stakingEndpoints staking = forever $ handleError logError $ awaitPromise
                           feedEndpoint
  where
    feedEndpoint :: Promise (Last Staking) StakingSchema Text ()
    feedEndpoint = endpoint @"feed" $ feed staking

userEndpoints :: Staking -> Contract (Last Staking) StakingSchema Text ()
userEndpoints staking = forever $ handleError logError $ awaitPromise $
                        registerEndpoint   `select`
                        unregisterEndpoint `select`
                        depositEndpoint    `select`
                        withdrawEndpoint   `select`
                        claimEndpoint      `select`
                        compoundEndpoint
  where
    registerEndpoint :: Promise (Last Staking) StakingSchema Text ()
    registerEndpoint = endpoint @"register" (const $ register staking)

    unregisterEndpoint :: Promise (Last Staking) StakingSchema Text ()
    unregisterEndpoint = endpoint @"unregister" (const $ unregister staking)

    depositEndpoint :: Promise (Last Staking) StakingSchema Text ()
    depositEndpoint = endpoint @"deposit" $ deposit staking

    withdrawEndpoint :: Promise (Last Staking) StakingSchema Text ()
    withdrawEndpoint = endpoint @"withdraw" $ withdraw staking

    claimEndpoint :: Promise (Last Staking) StakingSchema Text ()
    claimEndpoint = endpoint @"claim" $ const $ claim staking

    compoundEndpoint :: Promise (Last Staking) StakingSchema Text ()
    compoundEndpoint = endpoint @"compound" $ const $ compound staking

-- Off-chain code.
start
    :: MainToken
    -> StakingSettings
    -> Contract (Last Staking) s Text Staking
start am sett = do
    ownPKH   <- Contract.ownPubKeyHash
    nftCS    <- Currency.currencySymbol <$> forgeNFT ownPKH stakingNFTName

    let stakingAC   = assetClass nftCS stakingNFTName
        staking     = Staking { nft = stakingAC, settings  = sett }
        valNFT      = assetClassValue stakingAC 1
        val         = mainTokenValue $ getMicroToken am
        minAda      = Ada.toValue Ledger.minAdaTxOut

        lookupsInit =
               Constraints.typedValidatorLookups (typedValidatorStaking staking)
            <> Constraints.otherScript (validatorStaking staking)
        tx          = Constraints.mustPayToTheScript (mkPoolDatum []) $ val
                                                                     <> valNFT
                                                                     <> minAda

    submittedTx <- submitTxConstraintsWith lookupsInit tx
    void $ awaitTxConfirmed $ getCardanoTxId submittedTx
    logInfo @String $ unwords
        ["Staking pool has been created. Identifying NFT:"
        , show staking
        ]
    return staking

feed :: Staking -> MainToken -> Contract w s Text ()
feed staking am
    | getMicroToken am > 0 = do
        (orefStaking, oStaking) <- findStaking staking
        stakingDat              <- getContractDatum oStaking

        let newDat  = stakingDat
            oldVal  = getChainIndexTxOutValue oStaking
            newVal  = oldVal <> mainTokenValue (getMicroToken am)

            red     = feedRedeemer am
            lookups = mkLookups staking [(orefStaking, oStaking)]
            tx      =
                   Constraints.mustSpendScriptOutput orefStaking red
                <> Constraints.mustPayToTheScript newDat newVal

        submittedTx <- submitTxConstraintsWith lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId submittedTx
        logInfo @String $ unwords
            [ "Staking fed with", show am, "micro MyToken tokens." ]
    | otherwise = logInfo @String
        $ "Rejecting transaction which tries to feed a negative amount."

register :: Staking -> Contract w s Text ()
register staking = do
    (orefStaking, oStaking) <- findStaking staking
    activeUsers             <- getPoolState oStaking
    ownPKH                  <- Contract.ownPubKeyHash
    userNFTCS               <- Currency.currencySymbol <$>
                               forgeNFT ownPKH userNFTName

    let userNFT    = assetClass userNFTCS userNFTName
        stakingDat = PoolDatum $ Business.register activeUsers ownPKH userNFT
        stakingVal = getChainIndexTxOutValue oStaking
        userState  = mkUserDatum ownPKH [] Nothing
        userVal    = assetClassValue (assetClass userNFTCS userNFTName) 1
        userMinAda = Ada.toValue Ledger.minAdaTxOut

        red        = registerRedeemer ownPKH userNFT
        lookups    = mkLookups staking [(orefStaking, oStaking)]
        tx         = Constraints.mustSpendScriptOutput orefStaking red
                  <> Constraints.mustPayToTheScript stakingDat stakingVal
                  <> Constraints.mustPayToTheScript userState
                                                    (userVal <> userMinAda)

    submittedTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId submittedTx
    logInfo @String $ unwords [ "User with public key hash"
                              , show ownPKH
                              , "has been registered to the staking pool."
                              ]

unregister :: Staking -> Contract w s Text ()
unregister staking = do
    ownPKH                  <- Contract.ownPubKeyHash
    (orefStaking, oStaking) <- findStaking staking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    activeUsers             <- getPoolState oStaking
    userNFT                 <- getUserNFT staking ownPKH

    if not $ ownPKH `Business.isRegistered` activeUsers
    then logInfo @String "User is not currently registered."
    else if getChainIndexTxOutValue oUser /= (assetClassValue userNFT 1
                                           <> Ada.toValue Ledger.minAdaTxOut)
    then logInfo @String
        $ "User must remove all assets in the script before unregistering."
    else do
        let newStakingDat = PoolDatum $ Business.unregister activeUsers ownPKH
            newStakingVal = getChainIndexTxOutValue oStaking
            oldUserVal    = getChainIndexTxOutValue oUser
            minusMinAda   = Ada.toValue (-Ledger.minAdaTxOut)
            newUserVal    = oldUserVal <> assetClassValue userNFT (-1)
                                       <> minusMinAda

            red           = unregisterRedeemer ownPKH
            lookups       = mkLookups staking [ (orefStaking, oStaking)
                                              , (orefUser, oUser)
                                              ]
            tx            =
                   Constraints.mustSpendScriptOutput orefStaking red
                <> Constraints.mustSpendScriptOutput orefUser red
                <> Constraints.mustPayToTheScript newStakingDat newStakingVal
                <> Constraints.mustPayToPubKey ownPKH newUserVal

        submittedTx <- submitTxConstraintsWith lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId submittedTx
        logInfo @String $ unwords
            [ "User with public key hash"
            , show ownPKH
            , "has been unregistered from the staking pool."
            ]

deposit :: Staking -> MainToken -> Contract w s Text ()
deposit staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let depositRes = Business.deposit oldUserState (getMicroToken am)
                                      cTime (opSettings settings)

    case depositRes of
        Nothing -> logInfo @String  $
                   "Intended amount is lower than minimum allowed for deposits."
        Just (newUserState,feesD) -> do
            let refFees = Business.refillFees feesD
                dFees   = Business.daoFees feesD
                aFees   = Business.affFees feesD

                oldUserVal   = getChainIndexTxOutValue oUser
                totalFees    = refFees + dFees + aFees
                deposited    = getMicroToken am - totalFees
                newUserVal   = oldUserVal <> mainTokenValue deposited
                newUserDatum = UserDatum newUserState
                minAda       = Ada.toValue Ledger.minAdaTxOut

                range        = interval cTime (cTime + Business.validTimeRange)
                red          = depositRedeemer am cTime

                lookups      = mkLookups staking [ (orefUser, oUser) ]

                tx =  Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range
                   <> Constraints.mustPayToPubKey
                        (refWallet settings) (mainTokenValue refFees <> minAda)
                   <> Constraints.mustPayToPubKey
                        (daoWallet settings) (mainTokenValue dFees <> minAda)
                   <> Constraints.mustPayToPubKey
                        (affWallet settings) (mainTokenValue aFees <> minAda)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ unwords
                [ "User deposited", show am, "micro MyToken to their"
                , "script UTxO, and paid", show totalFees
                , "micro MyToken in fees."
                ]

withdraw :: Staking -> MainToken -> Contract w s Text ()
withdraw staking@Staking{..} am = do
    cTime             <- currentTime
    ownPKH            <- Contract.ownPubKeyHash
    (orefUser, oUser) <- findUserUTxO staking ownPKH
    oldUserState      <- getUserState oUser

    let withdrawRes = Business.withdraw oldUserState (getMicroToken am)
                                        (opSettings settings)
    case withdrawRes of
        Nothing -> logInfo @String  $
                "Intended amount is lower than minimum allowed for withdrawals."
        Just (newUserState,feesW) -> do
            let refFees = Business.refillFees feesW
                dFees   = Business.daoFees feesW
                aFees   = Business.affFees feesW

                oldUserVal   = getChainIndexTxOutValue oUser
                newUserVal   = oldUserVal <> mainTokenValue (- getMicroToken am)
                newUserDatum = UserDatum newUserState
                minAda       = Ada.toValue Ledger.minAdaTxOut

                range        = interval cTime (cTime + Business.validTimeRange)
                red          = withdrawRedeemer am cTime

                lookups      = mkLookups staking [ (orefUser, oUser) ]

                tx =  Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range
                   <> Constraints.mustPayToPubKey
                       (refWallet settings) (mainTokenValue refFees <> minAda)
                   <> Constraints.mustPayToPubKey
                       (daoWallet settings) (mainTokenValue dFees <> minAda)
                   <> Constraints.mustPayToPubKey
                       (affWallet settings) (mainTokenValue aFees <> minAda)
                   <> Constraints.mustPayToPubKey
                        ownPKH (mainTokenValue (getMicroToken am) <> minAda)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ unwords
                [ "User withdraw", show am
                , "micro MyToken from their script UTxO, and paid"
                , show (refFees + dFees + aFees)
                , "micro MyToken in fees."
                ]

claim :: Staking -> Contract w s Text ()
claim staking@Staking{..} = do
    cTime                   <- currentTime
    ownPKH                  <- Contract.ownPubKeyHash
    (orefStaking, oStaking) <- findStaking staking
    activeUsers             <- getPoolState oStaking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    oldUserState            <- getUserState oUser

    let claimRes = Business.claim oldUserState cTime (opSettings settings)

    case claimRes of
        Nothing -> logInfo @String $
                      "Claimable rewards do not yet reach the minimum ammount."
        Just (newUserState, rews) -> do
            void $ checkMinFundsPoolUTxO staking rews
            let oldStakingVal = getChainIndexTxOutValue oStaking
                newStakingVal = oldStakingVal <> mainTokenValue (-rews)
                newStakingDat = PoolDatum activeUsers
                newUserDatum  = UserDatum newUserState
                newUserVal    = getChainIndexTxOutValue oUser
                minAda        = Ada.toValue Ledger.minAdaTxOut

                range         = interval cTime (cTime + Business.validTimeRange)
                red           = claimRedeemer
                                   (MicroToken { getMicroToken = rews })
                                   cTime

                lookups       = mkLookups staking [ (orefStaking, oStaking)
                                                  , (orefUser, oUser)
                                                  ]

                tx =  Constraints.mustSpendScriptOutput orefStaking red
                   <> Constraints.mustSpendScriptOutput orefUser red
                   <> Constraints.mustPayToTheScript newStakingDat newStakingVal
                   <> Constraints.mustPayToTheScript newUserDatum newUserVal
                   <> Constraints.mustValidateIn range
                   <> Constraints.mustPayToPubKey
                        ownPKH (mainTokenValue rews <> minAda)

            submittedTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx
            logInfo @String $ unwords
                [ "User has claimed their rewards ("
                , show rews
                , "micro MyToken)."
                ]

compound :: Staking -> Contract w s Text ()
compound staking@Staking{..} = do
    cTime                   <- currentTime
    ownPKH                  <- Contract.ownPubKeyHash
    (orefStaking, oStaking) <- findStaking staking
    (orefUser, oUser)       <- findUserUTxO staking ownPKH
    activeUsers             <- getPoolState oStaking
    oldUserState            <- getUserState oUser

    let compoundRes = Business.compound oldUserState cTime (opSettings settings)

    case compoundRes of
        Nothing -> logInfo @String $
                      "Claimable rewards do not yet reach the minimum ammount."
        Just (newUserState, rews) -> do
            void $ checkMinFundsPoolUTxO staking rews
            let oldStakingVal = getChainIndexTxOutValue oStaking
                newStakingVal = oldStakingVal <> mainTokenValue (-rews)
                newStakingDat = PoolDatum activeUsers
                newUserDatum  = UserDatum newUserState

                oldUserVal    = getChainIndexTxOutValue oUser
                newUserVal    = oldUserVal <> mainTokenValue rews

                range         = interval cTime (cTime + Business.validTimeRange)
                red           = compoundRedeemer
                                    (MicroToken { getMicroToken = rews })
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
            logInfo @String $ unwords
                [ "User has compounded their rewards ("
                , show rews
                , "micro MyToken)."
                ]

-- Helper functions.
mkLookups
    :: Staking
    -> [(TxOutRef, ChainIndexTxOut)]
    -> ScriptLookups StakingType
mkLookups p utxos = Constraints.typedValidatorLookups (typedValidatorStaking p)
                 <> Constraints.otherScript (validatorStaking p)
                 <> Constraints.unspentOutputs (Map.fromList utxos)

-- | Monadic function returning the UTxO corresponding to the staking pool.
findStaking
    :: Staking
    -> Contract w s Text (TxOutRef, ChainIndexTxOut)
findStaking staking@Staking{..} = lookupScriptUTxO (addressStaking staking) nft

-- | Monadic function for getting the datum from a staking pool UTxO.
getPoolState
    :: ChainIndexTxOut
    -> Contract w s T.Text Business.PoolState
getPoolState o =
    case getChainIndexTxOutDatum o of
        Just (PoolDatum ps) -> return ps
        Just (UserDatum _)  ->
            throwError "Expected StakingDatum but found UserDatum."
        Nothing -> throwError "Cannot find contract datum."

{- | Monadic function returning the user script UTxO corresponding to the
     PubKeyHash. -}
findUserUTxO
    :: Staking
    -> PubKeyHash
    -> Contract w s Text (TxOutRef, ChainIndexTxOut)
findUserUTxO staking pkh = do
    (_, oStaking) <- findStaking staking
    dat           <- getContractDatum oStaking
    case dat of
        UserDatum _             -> throwError
            "Expected StakingDatum but found UserDatum in staking script UTxO."
        PoolDatum ps ->
            case Business.getUserNFT ps pkh of
                Just userNFT -> lookupScriptUTxO
                                    (addressStaking staking)
                                    userNFT
                _            -> throwError "Could not find user NFT."

{- | Monadic function for getting the AssetClass associated with the
     PubKeyHash value in the Staking value. -}
getUserNFT
    :: Staking
    -> PubKeyHash
    -> Contract w s Text AssetClass
getUserNFT staking@Staking{..} pkh = do
    (_, oStaking)  <- lookupScriptUTxO (addressStaking staking) nft
    activeUsers    <- getContractDatum oStaking
    case activeUsers of
        UserDatum _             -> throwError
            "Expected StakingDatum but found UserDatum in staking script UTxO."
        PoolDatum ps ->
            case Business.getUserNFT ps pkh of
                Just userNFT -> return userNFT
                _            -> throwError "Could not find user NFT."

-- | Monadic function for getting the UserState from a user script UTxO.
getUserState
    :: ChainIndexTxOut
    -> Contract w s T.Text Business.UserState
getUserState o = case getChainIndexTxOutDatum o of
    Just dat -> case dat of
        PoolDatum _   ->
            throwError "Expected UserDatum but found StakingDatum."
        UserDatum res -> return res
    Nothing  -> throwError "Cannot find contract datum."

-- | Monadic function for getting the datum from a ChainIndexTxOut.
getContractDatum
    :: ChainIndexTxOut
    -> Contract w s T.Text StakingDatum
getContractDatum =
    maybe (throwError "Cannot find contract datum") return .
          getChainIndexTxOutDatum

{- | Monadic function for checking if there is enough funds in pool UTxO for a
     claim or compound transaction -}
checkMinFundsPoolUTxO
    :: Staking
    -> Integer
    -> Contract w s T.Text ()
checkMinFundsPoolUTxO staking rews = do
    (_, oStaking) <- findStaking staking
    when
      (assetClassValueOf (getChainIndexTxOutValue oStaking) mainTokenAC < rews)
      $ throwError $ pack $ unwords
        [ "Claim or Compound transaction not issued due to"
        , "unsufficient funds in pool UTxO."
        ]
