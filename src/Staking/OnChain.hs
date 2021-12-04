{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-|
Module      : Staking.OnChain
Description : On-Chain code of the staking pool.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.OnChain (mkValidatorStaking) where

-- Third-party libraries.
import Ledger
import Ledger.Ada      as Ada
import Ledger.Value
import PlutusTx.Prelude

-- Internal modules.
import Staking.Business as Business
import Staking.Types
import MainToken
import Utils.ScriptContext


{-# INLINABLE mkValidatorStaking #-}
mkValidatorStaking
    :: AssetClass
    -> Staking
    -> StakingDatum
    -> StakingRedeemer
    -> ScriptContext
    -> Bool
mkValidatorStaking ac staking (PoolDatum pstate) (Feed am) ctx =
    validateFeed ac staking pstate am ctx
mkValidatorStaking _ staking (PoolDatum pstate) (Register pkh userNFT) ctx =
    validatePoolRegister staking pstate pkh userNFT ctx
mkValidatorStaking _ staking (PoolDatum pstate) (Unregister pkh) ctx =
    validatePoolUnregister staking pstate pkh ctx
mkValidatorStaking _ _ (UserDatum _) (Unregister pkh) ctx =
    validateUserUnregister pkh ctx
mkValidatorStaking ac staking (UserDatum ustate) (Deposit am time) ctx =
    validateDeposit ac staking ustate am time ctx
mkValidatorStaking ac staking (UserDatum ustate) (Withdraw am time) ctx =
    validateWithdraw ac staking ustate am time ctx
mkValidatorStaking ac staking (PoolDatum pstate) (Claim rews time) ctx =
    validatePoolClaim ac staking pstate rews time ctx
mkValidatorStaking _ staking (UserDatum ustate) (Claim _ time) ctx =
    validateUserClaim staking ustate time ctx
mkValidatorStaking ac staking (PoolDatum pstate) (Compound rews time) ctx =
    validatePoolCompound ac staking pstate rews time ctx
mkValidatorStaking ac staking (UserDatum ustate) (Compound rews time) ctx =
    validateUserCompound ac staking ustate rews time ctx
mkValidatorStaking _ _ _ _ _ = False

{- | Validation for Feed action when the Pool utxo is spent.
     We check that:
        * the input value contains the pool utxo NFT,
        * the output value is equal to the input value plus the specified
          amount of MainTokens,
        * the datum doesn't change.
-}
{-# INLINABLE validateFeed #-}
validateFeed
    :: AssetClass
    -> Staking
    -> PoolState
    -> MainToken
    -> ScriptContext
    -> Bool
validateFeed ac staking pstate am ctx =
       traceIfFalse
            "checkPoolFeed: NFT missing from the input UTxO."
            (inputHasNFT (nft staking) ctx)
    && traceIfFalse
            "checkPoolFeed: Output value is wrong."
            checkFeededValue
    && traceIfFalse
            "checkPoolFeed: Datum should not have changed."
            (PoolDatum pstate == outputDatum)
  where
    inputValue :: Value
    inputValue =
        maybe (traceError "checkPoolFeed: Input UTxO could not be found.")
              (txOutValue . txInInfoResolved)
              (findOwnInput ctx)

    outputValue :: Value
    outputValue =
        maybe (traceError "checkPoolFeed: Output UTxO could not be found.")
              txOutValue
              (uniqueScriptOutput ctx)

    checkFeededValue :: Bool
    checkFeededValue = let feededAmount = getMicroToken am in
           feededAmount > 0
        && inputValue <> assetClassValue ac feededAmount == outputValue

    outputDatum :: StakingDatum
    outputDatum =
        fromMaybe (traceError "checkPoolFeed: Output datum could not be found.")
                  (uniqueScriptOutput ctx >>= (`getTxDatum` ctx))

{- | Validation for Register action when the Pool utxo is spent.
     We check that:
        * the transaction is signed by pkh,
        * the output pool UTxO value is equal to the input value,
        * the output pool UTxO datum is equal to that of the input plus the
            newly registered user information, which implies checking that the
            value of the newly created user UTxO equals the NFT created for
            pkh alone, and that
        * the datum of the user UTxO is as expected.
-}
{-# INLINABLE validatePoolRegister #-}
validatePoolRegister
    :: Staking
    -> PoolState
    -> PubKeyHash
    -> AssetClass
    -> ScriptContext
    -> Bool
validatePoolRegister staking inputPoolState pkh userNFT ctx =
       traceIfFalse
            "checkPoolRegister: Transaction not signed by the right user."
            signedByRightUser
    && traceIfFalse
            ("checkPoolRegister: Pool UTxO value expected to remain the same."
                `appendString` " but changed.")
            (unchangedValueFromNFT ctx (nft staking))
    && traceIfFalse
            "checkPoolRegister: Pool UTxO datum is wrong."
            checkOutputDatum
    && traceIfFalse
            "checkPoolRegister: User UTxO datum is wrong."
            checkNewUserUTxO
  where
    signedByRightUser :: Bool
    signedByRightUser = scriptContextTxInfo ctx `txSignedBy` pkh

    outputPoolState :: PoolState
    outputPoolState =
        case getTxOutFromNFT ctx (nft staking) >>= (`getTxDatum` ctx) of
            Just (PoolDatum res) -> res
            _                    -> traceError $ "checkPoolRegister: Pool UTxO "
                                        `appendString` "could not be found."

    checkOutputDatum :: Bool
    checkOutputDatum =
        outputPoolState == Business.register inputPoolState pkh userNFT

    checkNewUserUTxO :: Bool
    checkNewUserUTxO =
       case getTxOutFromNFT ctx userNFT >>= (`getTxDatum` ctx) of
           Just (UserDatum userState) -> userState == mkUserState pkh [] Nothing
           _                          -> False


{- | Validation for Unregister action when the Pool utxo is spent.
     In the following case, from the standpoint of the pool script UTxO, we
     must check that:
        * the transaction is signed by pkh,
        * the output pool UTxO value equals that of the input,
        * the output pool UTxO datum is equal to that of the input minus the
            information of the user being unregistered.
-}
{-# INLINABLE validatePoolUnregister #-}
validatePoolUnregister
    :: Staking
    -> PoolState
    -> PubKeyHash
    -> ScriptContext
    -> Bool
validatePoolUnregister staking inputPoolState pkh ctx =
       traceIfFalse
            "checkPoolUnregister: Transaction not signed by the right user."
            signedByRightUser
    && traceIfFalse
            ("checkPoolUnregister: A user UTxO containing the right NFT could "
                `appendString` "not be found.")
            checkUserIsRegistered
    && traceIfFalse
            ("checkPoolUnregister: Pool UTxO value expected to remain the same."
                `appendString` " but changed.")
            (unchangedValueFromNFT ctx (nft staking))
    && traceIfFalse
            "checkPoolUnregister: Pool ouput UTxO datum is wrong."
            checkOutputDatum
  where
    signedByRightUser :: Bool
    signedByRightUser = scriptContextTxInfo ctx `txSignedBy` pkh

    checkUserIsRegistered :: Bool
    checkUserIsRegistered = isJust $ inputPoolState `Business.getUserNFT` pkh

    checkOutputDatum :: Bool
    checkOutputDatum =
        outputPoolState == Business.unregister inputPoolState pkh

    outputPoolState :: PoolState
    outputPoolState =
        case getTxOutFromNFT ctx (nft staking) >>= (`getTxDatum` ctx) of
            Just (PoolDatum res) -> res
            _                    -> traceError $ "checkPoolRegister: Pool UTxO "
                                        `appendString` "could not be found."

{- | Validation for Unregister action when the User utxo is spent.
     We must check that:
        * the transaction is signed by pkh,
        * the user UTxO cannot be found in the outputs, and that
        * the NFT has been burnt (TBA).
-}
{-# INLINABLE validateUserUnregister #-}
validateUserUnregister
    :: PubKeyHash
    -> ScriptContext
    -> Bool
validateUserUnregister pkh ctx =
       traceIfFalse
            "checkUserUnregister: Transaction not signed by the right user."
            signedByRightUser
    && traceIfFalse
            "checkUserUnregister: The user UTxO could be found."
            checkUserOutputCannotBeFound
    && traceIfFalse
            "checkUserUnregister: The user UTxO NFT should have been burnt."
            True
            {- TODO: Implement the action of burning the NFT when a user is
                     unregistered and change 'True' by the corresponding check.
            -}
  where
    signedByRightUser :: Bool
    signedByRightUser = scriptContextTxInfo ctx `txSignedBy` pkh

    checkUserOutputCannotBeFound :: Bool
    checkUserOutputCannotBeFound = case getContinuingOutputs ctx of
        [o] -> case getTxDatum o ctx of
            Just (PoolDatum _) -> True
            _                  -> False
        _   -> False

{- | Validation for Deposit action when the User utxo is spent.
     We must check that:
        * the transaction is signed by the pub key specified in the user state,
        * the validation time range is not longer than a predefined range and
          the deposit time is inside it,
        * the deposit can be performed according to the staking business logic,
        * the value blocked in the utxo coincides with the expected one,
        * the tokens in concept of fees are distributed properly,
        * the output user state is the expected one.
-}
{-# INLINABLE validateDeposit #-}
validateDeposit
    :: AssetClass
    -> Staking
    -> UserState
    -> MainToken
    -> POSIXTime
    -> ScriptContext
    -> Bool
validateDeposit ac staking@Staking{..} inputUserState am time ctx =
       traceIfFalse
            "checkUserDeposit: Transaction not signed by the right user."
            signedByRightUser
    && traceIfFalse
            "checkUserDeposit: Transaction time is wrong."
            (checkTimeRange ctx time)
    && traceIfFalse
            "checkUserDeposit: Amount to deposit does not reach minimum."
            (isJust depositRes)
    && traceIfFalse
            "checkUserDeposit: Output value is wrong."
            (outputValue == inputValue <> assetClassValue ac depositAm)
    && checkFeesDistribution ac staking ref dao aff ctx
    && traceIfFalse
            "checkUserDeposit: Datum is wrong."
            checkOutputDatum
  where
    signedByRightUser :: Bool
    signedByRightUser = scriptContextTxInfo ctx `txSignedBy` user inputUserState

    checkOutputDatum :: Bool
    checkOutputDatum =
           time `Business.isAfter` inputUserState
        && outputUserState == newUserState

    inputValue :: Value
    inputValue =
        maybe (traceError "checkUserDeposit: Input UTxO could not be found.")
              (txOutValue . txInInfoResolved)
              (findOwnInput ctx)

    outputValue :: Value
    outputValue =
        maybe (traceError "checkUserDeposit: Output UTxO could not be found.")
              txOutValue
              (uniqueScriptOutput ctx)

    depositRes :: Maybe (UserState, FeesDistribution)
    depositRes = Business.deposit inputUserState (getMicroToken am) time
                                  (opSettings settings)

    newUserState :: UserState
    feesD :: FeesDistribution
    (newUserState, feesD) = fromJust depositRes

    ref, dao, aff :: Integer
    (ref, dao, aff) = (Business.refillFees feesD,
                       Business.daoFees    feesD,
                       Business.affFees    feesD)

    depositAm :: Integer
    depositAm = getMicroToken am - ref - dao - aff

    outputUserState :: UserState
    outputUserState = case outputDatum of
        UserDatum res -> res
        _             ->
            traceError $ "checkUserDeposit: Output user UTxO datum could not be"
                                                        `appendString` " found."

    outputDatum :: StakingDatum
    outputDatum =
        fromMaybe (traceError $ "checkUserDeposit: Output datum could not be "
                                    `appendString` "found.")
                  (uniqueScriptOutput ctx >>= (`getTxDatum` ctx))



{- | Validation for Withdraw action when the User utxo is spent.
     We must check that:
        * the transaction is signed by the pub key specified in the user state,
        * the validation time range is not longer than a predefined range and
          the withdraw time is inside it,
        * the withdraw can be performed according to the staking business logic,
        * the value blocked in the utxo coincides with the expected one,
        * the tokens in concept of fees are distributed properly,
        * the output user state is the expected one.
-}
{-# INLINABLE validateWithdraw #-}
validateWithdraw
    :: AssetClass
    -> Staking
    -> UserState
    -> MainToken
    -> POSIXTime
    -> ScriptContext
    -> Bool
validateWithdraw ac staking@Staking{..} inputUserState am time ctx =
       traceIfFalse
            "checkUserWithdraw: Transaction not signed by the right user."
            signedByRightUser
    && traceIfFalse
            "checkUserWithdraw: Transaction time is wrong."
            (checkTimeRange ctx time)
    && traceIfFalse
            "checkUserWithdraw: Amount to withdraw does not reach minimum."
            (isJust withdrawRes)
    && traceIfFalse
            "checkUserWithdraw: Output value is wrong."
            (outputValue == inputValue <> withdrawValue)
    && checkFeesDistribution ac staking ref dao aff ctx
    && traceIfFalse
            "checkUserWithdraw: Datum is wrong."
            checkOutputDatum
  where
    signedByRightUser :: Bool
    signedByRightUser = scriptContextTxInfo ctx `txSignedBy` user inputUserState

    inputValue :: Value
    inputValue =
        maybe (traceError "checkUserWithdraw: Input UTxO could not be found.")
              (txOutValue . txInInfoResolved)
              (findOwnInput ctx)

    outputValue :: Value
    outputValue =
        maybe (traceError $ "checkUserWithdraw: Output UTxO could not be "
                                `appendString` "found.")
              txOutValue
              (uniqueScriptOutput ctx)

    withdrawRes :: Maybe (UserState, FeesDistribution)
    withdrawRes = Business.withdraw inputUserState (getMicroToken am)
                                    (opSettings settings)

    newUserState :: UserState
    feesD :: FeesDistribution
    (newUserState, feesD) = fromJust withdrawRes

    ref, dao, aff :: Integer
    (ref, dao, aff) = (Business.refillFees feesD,
                       Business.daoFees    feesD,
                       Business.affFees    feesD)

    withdrawValue :: Value
    withdrawValue = assetClassValue ac $ negate $ getMicroToken am

    checkOutputDatum :: Bool
    checkOutputDatum =
           time `Business.isAfter` inputUserState
        && outputUserState == newUserState

    outputUserState :: UserState
    outputUserState = case outputDatum of
        UserDatum res -> res
        _             ->
            traceError $ "checkUserDeposit: Output user UTxO datum could not be"
                                                        `appendString` " found."

    outputDatum :: StakingDatum
    outputDatum =
        fromMaybe (traceError $ "checkUserWithdraw: Output datum could not be "
                                    `appendString` "found.")
                  (uniqueScriptOutput ctx >>= (`getTxDatum` ctx))



{- | Validation for Claim action when the Pool utxo is spent.
     We check that:
        * the transaction is signed by someone who is registered to the
            staking pool,
        * the validation time range is not longer than a predefined range and
          the claim time is inside it,
        * the transaction is consuming a single user UTXO (this is checked when
            calculating the rewards, particularly when obtaining the user input
            UTxO in inputUserState),
        * the output pool UTxO value equals that of the input minus the rewards,
        * the output pool UTxO datum is equal to that of the input.
-}
{-# INLINABLE validatePoolClaim #-}
validatePoolClaim
    :: AssetClass
    -> Staking
    -> PoolState
    -> MainToken
    -> POSIXTime
    -> ScriptContext
    -> Bool
validatePoolClaim ac staking inputPoolState rews time ctx =
       traceIfFalse
            "checkPoolClaim: Transaction signer is not registered."
            signerIsRegistered
    && traceIfFalse
            "checkPoolClaim: Transaction time is wrong."
            (checkTimeRange ctx time)
    && traceIfFalse
            "checkPoolClaim: Claimed rewards do not reach minimum ammount."
            (isJust claimRes)
    && traceIfFalse
            "checkPoolClaim: Claimed rewards are wrong."
            checkComputedRewards
   && traceIfFalse
            "checkPoolClaim: Pool ouput UTxO value is wrong."
            (inputValue <> rewsValue == outputValue)
    && traceIfFalse
            "checkPoolClaim: Pool ouput UTxO datum is wrong."
            (inputPoolState == outputPoolState)
  where
    signerIsRegistered :: Bool
    signerIsRegistered = signer `Business.isRegistered` inputPoolState

    signer :: PubKeyHash
    signer = case txInfoSignatories (scriptContextTxInfo ctx) of
        [pkh] -> pkh
        _     -> traceError "Signer could not be found."

    claimRes :: Maybe (UserState, Integer)
    claimRes = Business.claim inputUserState time (opSettings $ settings staking)

    rewards :: Integer
    (_,rewards) = fromJust claimRes

    checkComputedRewards :: Bool
    checkComputedRewards = getMicroToken rews == rewards

    inputUserState :: UserState
    inputUserState = case getAllScriptInputs ctx of
        scriptIns@[_, _] ->
            case mapMaybe (findInputUserState . txInInfoResolved) scriptIns of
                [userState] -> userState
                _           -> traceError
                    "checkPoolClaim: The number of user input UTxOs is wrong."
        _                 -> traceError
            "checkPoolClaim: The number of input UTxOs is wrong."

    findInputUserState :: TxOut -> Maybe UserState
    findInputUserState o = case getTxDatum o ctx of
        Just (UserDatum userState) -> Just userState
        _                          -> Nothing

    inputValue :: Value
    inputValue = case findOwnInput ctx of
        Just o  -> txOutValue (txInInfoResolved o)
        Nothing -> traceError $ "checkPoolClaim: Pool input UTxO could not be "
                        `appendString` "found."

    rewsValue :: Value
    rewsValue = negate $ assetClassValue ac $ getMicroToken rews

    outputValue :: Value
    outputValue = case getTxOutFromNFT ctx (nft staking) of
        Just o  -> txOutValue o
        Nothing -> traceError $ "checkPoolClaim: Pool output UTxO could not be "
                        `appendString` "found (when looking for its value)."

    outputPoolState :: PoolState
    outputPoolState = case getTxDatum outputPoolTxOut ctx of
        Just (PoolDatum poolState) -> poolState
        _                          ->
            traceError $ "checkPoolClaim: Pool output UTxO datum could not "
                `appendString` "be found."

    outputPoolTxOut :: TxOut
    outputPoolTxOut = case getTxOutFromNFT ctx (nft staking) of
        Just o  -> o
        Nothing -> traceError $ "checkPoolClaim: Pool output UTxO could not be "
                    `appendString` "found (when looking for its datum)."

{- | Validation for Claim action when the User utxo is spent.
     We check that:
        * the pool UTxO is spent.
        * the public key hash in the user input UTxO datum is that of the signer
            of the transaction,
        * the output user UTxO value equals that of the input plus the rewards,
        * the output user UTxO datum equals that of the input plus a ClaimTx
            entry with a valid time stamp,
-}
{-# INLINABLE validateUserClaim #-}
validateUserClaim
    :: Staking
    -> UserState
    -> POSIXTime
    -> ScriptContext
    -> Bool
validateUserClaim staking@Staking{..} inputUserState time ctx =
       traceIfFalse
            "checkUserClaim: Pool UTxO is not consumed."
            (checkPoolIsSpent ctx nft)
    && traceIfFalse
            "checkUserClaim: Wrong user UTxO."
            (signer == user inputUserState)
    && traceIfFalse
            "checkUserClaim: User UTxO value should not have changed."
            (txOutValue ownInput == txOutValue ownOutput)
    &&  traceIfFalse
            "checkUserClaim: User UTxO datum is wrong."
            checkOutputDatum
  where
    signer :: PubKeyHash
    signer = case txInfoSignatories (scriptContextTxInfo ctx) of
        [pkh] -> pkh
        _     -> traceError "Signer could not be found."

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Just inInfo -> txInInfoResolved inInfo
        Nothing     ->
            traceError "checkUserClaim: User input UTxO could not be found."

    ownOutput :: TxOut
    ownOutput = case findUserTxOut signer staking ctx of
        Just o  -> o
        Nothing ->
            traceError "checkUserClaim: User output UTxO could not be found."

    newUserState :: UserState
    (newUserState, _) = fromJust $ Business.claim inputUserState time
                                                  (opSettings settings)

    checkOutputDatum :: Bool
    checkOutputDatum =
           time `Business.isAfter` inputUserState
        && outputUserState == newUserState

    outputUserState :: UserState
    outputUserState = case getTxDatum ownOutput ctx of
        Just (UserDatum res) -> res
        _                    -> traceError $ "checkUserClaim: User state from "
                                    `appendString` "user output UTxO could not "
                                    `appendString` "be found."

{-|  In the following case, from the standpoint of the pool script UTxO, we
    must run the same checks than those of the claim operation, from the pool
    script UTxO standpoint too.
-}
{-# INLINABLE validatePoolCompound #-}
validatePoolCompound
    :: AssetClass
    -> Staking
    -> PoolState
    -> MainToken
    -> POSIXTime
    -> ScriptContext
    -> Bool
validatePoolCompound = validatePoolClaim


{- | Validation for Compound action when the User utxo is spent.
     We check that:
        * the pool UTxO is spent.
        * the public key hash in the user input UTxO datum is that of the signer
            of the transaction,
        * the output user UTxO value equals that of the input plus the rewards,
        * the output user UTxO datum is the expected one.
-}
{-# INLINABLE validateUserCompound #-}
validateUserCompound
    :: AssetClass
    -> Staking
    -> UserState
    -> MainToken
    -> POSIXTime
    -> ScriptContext
    -> Bool
validateUserCompound ac staking@Staking{..} inputUserState rews time ctx =
       traceIfFalse
            "checkUserCompound: Pool UTxO is not consumed."
            (checkPoolIsSpent ctx nft)
    && traceIfFalse
            "checkUserCompound: Transaction not signed by the right user."
            (user inputUserState == signer)
    && traceIfFalse
            "checkPoolCompound: Claimed rewards do not reach minimum ammount."
            (isJust compoundRes)
    && traceIfFalse
            "checkPoolCompound: Pool UTxO value is wrong."
            (inputValue <> assetClassValue ac (getMicroToken rews) == outputValue)
    && traceIfFalse
            "checkUserCompound: User output UTxO datum is wrong."
            checkOutputDatum
  where
    signer :: PubKeyHash
    signer = case txInfoSignatories (scriptContextTxInfo ctx) of
        [pkh] -> pkh
        _     -> traceError "Signer could not be found."

    inputValue :: Value
    inputValue = case findOwnInput ctx of
        Just inInfo -> txOutValue $ txInInfoResolved inInfo
        Nothing     ->
            traceError "checkUserCompound: User input UTxO could not be found."

    outputValue :: Value
    outputValue = txOutValue ownOutput

    ownOutput :: TxOut
    ownOutput = case findUserTxOut signer staking ctx of
        Just o  -> o
        Nothing ->
            traceError "checkUserCompound: User output UTxO could not be found."

    compoundRes :: Maybe (UserState, Integer)
    compoundRes = Business.compound inputUserState time (opSettings settings)

    newUserState :: UserState
    (newUserState, _) = fromJust compoundRes

    checkOutputDatum :: Bool
    checkOutputDatum =
           time `Business.isAfter` inputUserState
        && outputUserState == newUserState

    outputUserState :: UserState
    outputUserState = case getTxDatum ownOutput ctx of
        Just (UserDatum userState) -> userState
        _                          ->
            traceError "User output UTxO datum could not be found."


{-# INLINABLE checkFeesDistribution #-}
checkFeesDistribution ::
       AssetClass
    -> Staking
    -> Integer
    -> Integer
    -> Integer
    -> ScriptContext
    -> Bool
checkFeesDistribution ac Staking{..} refFees daoFees affFees ctx =
       traceIfFalse
            "checkFeesDistribution: Incentives pool fees are wrong."
            (valuePaidTo (scriptContextTxInfo ctx) (refWallet settings)
                == assetClassValue ac refFees <> minAda)
    && traceIfFalse
            "checkFeesDistribution: DAO fees are wrong."
            (valuePaidTo (scriptContextTxInfo ctx) (daoWallet settings)
                == assetClassValue ac daoFees <> minAda)
    && traceIfFalse
            "checkFeesDistribution: Affiliates fees are wrong."
            (valuePaidTo (scriptContextTxInfo ctx) (affWallet settings)
                == assetClassValue ac affFees <> minAda)
  where
    minAda :: Value
    minAda = Ada.toValue Ledger.minAdaTxOut

{-# INLINABLE checkPoolIsSpent #-}
checkPoolIsSpent :: ScriptContext -> AssetClass -> Bool
checkPoolIsSpent ctx ac = isJust $ findSpentTxOut ctx ac

-- | Obtains the continuing user UTxO corresponding to a PubKeyHash value.
{-# INLINABLE findUserTxOut #-}
findUserTxOut :: PubKeyHash -> Staking -> ScriptContext -> Maybe TxOut
findUserTxOut pkh staking ctx =
    case mPoolStateDatum of
        Just (PoolDatum ps) ->
            userNFT ps >>= getTxOutFromNFT ctx
        _                              -> Nothing
  where
    userNFT :: PoolState -> Maybe AssetClass
    userNFT = flip Business.getUserNFT pkh

    mPoolStateDatum :: Maybe StakingDatum
    mPoolStateDatum = findSpentTxOut ctx (nft staking) >>= (`getTxDatum` ctx)

{-# INLINABLE checkTimeRange #-}
checkTimeRange :: ScriptContext -> POSIXTime -> Bool
checkTimeRange ctx time =
    checkIntervalSize timeRange Business.validTimeRange
    -- XXX: +1 because timeRange lower closure may be False
    && (time + 1) `member` timeRange

  where timeRange :: Interval POSIXTime
        timeRange = txInfoValidRange (scriptContextTxInfo ctx)

        checkIntervalSize :: Interval POSIXTime -> POSIXTime -> Bool
        checkIntervalSize iv len =
            case getIvFrom iv of
                Just t  -> interval t (t + len) `contains` iv
                Nothing -> False

{-# INLINABLE getIvFrom #-}
getIvFrom :: Interval a -> Maybe a
getIvFrom iv = case ivFrom iv of
    LowerBound (Finite lBound) _ -> Just lBound
    _                            -> Nothing
-- | Function to return the unwrapped value of a transaction
{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just valueInfo) = valueInfo
fromJust Nothing = traceError
                   "fromJust Nothing"
