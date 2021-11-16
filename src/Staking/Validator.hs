{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DataKinds        #-}

{-|
Module      : Staking.Validator
Description : Boilerplate for compiling the plutus onchain code.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.Validator
    ( validatorStaking
    , typedValidatorStaking
    , addressStaking
    , StakingType
    ) where

import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           PlutusTx

import           MainToken
import           Staking.Types
import           Staking.OnChain

data StakingType
instance Scripts.ValidatorTypes StakingType where
    type instance DatumType    StakingType = StakingDatum
    type instance RedeemerType StakingType = StakingRedeemer

typedValidatorStaking :: Staking -> Scripts.TypedValidator StakingType
typedValidatorStaking staking = Scripts.mkTypedValidator @StakingType
    ($$(PlutusTx.compile [|| mkValidatorStaking ||])
            `PlutusTx.applyCode` PlutusTx.liftCode mainTokenAC
            `PlutusTx.applyCode` PlutusTx.liftCode staking)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @StakingDatum @StakingRedeemer

validatorStaking :: Staking -> Scripts.Validator
validatorStaking = Scripts.validatorScript . typedValidatorStaking

addressStaking :: Staking -> Ledger.Address
addressStaking = scriptAddress . validatorStaking
