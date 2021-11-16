{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeApplications   #-}

{-|
Module      : Tests.TestUtils
Description : Common tests utils functions.
Copyright   : (c) 2021 IDYIA LLC dba imagine.ai
Maintainer  : sos@imagine.ai
Stability   : develop
-}

module Tests.TestUtils where

-- GHC libraries.
import qualified Control.Foldl         as L
import           Control.Monad
import           Control.Lens               ((.~))
import           Control.Monad.Freer.Writer (tell)

import           Data.Function         ((&))
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Data.Default          (Default (..))
import           Data.Map              as Map hiding (take)
import           Data.Monoid           (Last (..))
import           Data.Text

import           Ledger
import           Ledger.Ada            as Ada
import           Ledger.Value          as Value
import           Plutus.Trace.Emulator as Emulator
import           Plutus.Contract.Test
import           Wallet.Emulator.Folds      (postMapM, failedTransactions)

import           Test.Tasty

import           Staking.Business
import           Staking.OffChain
import           Staking.Tokens
import           Staking.Types
import           MainToken

runTrace :: EmulatorTrace () -> IO ()
runTrace = Emulator.runEmulatorTraceIO' def emCfg

emCfg :: Emulator.EmulatorConfig
emCfg = Emulator.EmulatorConfig
            { _initialChainState =
                    Left $ Map.fromList $ [(wi, v)| wi <- Prelude.take 10 knownWallets]
            , _slotConfig = def
            , _feeConfig = def
            }
    where
      v :: Value
      v =    Ada.lovelaceValueOf 10_000_000
          <> mainTokenValue          1_000_000_000

getStaking ::
       ContractHandle (Last Staking) StakingSchema Text
    -> EmulatorTrace Staking
getStaking h = do
    void $ Emulator.waitNSlots 1
    Last m <- observableState h
    case m of
        Nothing   -> getStaking h
        Just pool -> return pool

negativeTokenOf :: MainToken -> MainToken
negativeTokenOf MicroToken{..} = MicroToken { getMicroToken = -getMicroToken }

-- | Assert that exactly one transaction failed to validate, and this
--   transaction that failed meet the predicate.
assertFailedExactOneTx
    :: (Tx -> ValidationError -> [ScriptValidationEvent] -> Bool)
    -> TracePredicate
assertFailedExactOneTx predicate =
    flip postMapM (L.generalize $ failedTransactions Nothing) $ \case
    [(_, t, e, evts)] -> return $ predicate t e evts
    [] -> tell @(Doc Void) "No transactions failed to validate."
          >> return False
    _ -> tell @(Doc Void) "More than one transactions failed to validate."
         >> return False

buildTest :: String -> [ScriptError] -> EmulatorTrace () -> TestTree
buildTest msg errs =
    checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    msg
    (assertFailedExactOneTx failedTx)
  where
    failedTx :: Tx -> ValidationError -> [ScriptValidationEvent] -> Bool
    failedTx _ (ScriptFailure scriptError) _ = scriptError `elem` errs
    failedTx _ _ _ = False

wallet :: Int -> Wallet
wallet i = knownWallets !! i

p2pWallet :: Wallet
p2pWallet = wallet 0

user1Wallet :: Wallet
user1Wallet = wallet 1

user1WalletPKH :: PubKeyHash
user1WalletPKH = pubKeyHash $ walletPubKey user1Wallet

testStakingNFT :: Value
testStakingNFT = Value.singleton testStakingNFTCS stakingNFTName 1

testStakingNFTCS :: CurrencySymbol
testStakingNFTCS = "d43f9500dbc82887d5d8bc9dd8b6b1f61e5fe9709e7109f5f6ef835a"

testUserNFT :: Value
testUserNFT = Value.singleton testUserNFTCS userNFTName 1

testUserNFTCS :: CurrencySymbol
testUserNFTCS = "7bd2a17106dad5472a6e1aec09e7f3db705528f35e209fdfc1ca86fa"

testStaking :: Staking
testStaking = mkStaking (assetClass testStakingNFTCS stakingNFTName)
                        testStakingSettings

testClaimStaking :: Staking
testClaimStaking = mkStaking (assetClass testStakingNFTCS stakingNFTName)
                        testStakingClaimSettings

walletPKH :: Wallet -> PubKeyHash
walletPKH w = pubKeyHash $ walletPubKey w

testRefWallet, testDaoWallet, testAffWallet :: Wallet
testRefWallet = wallet 7
testDaoWallet = wallet 8
testAffWallet = wallet 9

testStakingSettings :: StakingSettings
testStakingSettings = StakingSettings
    { refWallet   = walletPKH testRefWallet
    , daoWallet   = walletPKH testDaoWallet
    , affWallet   = walletPKH testAffWallet
    , opSettings  = testOperationSettings
    }

testOperationSettings :: OperationSettings
testOperationSettings = OperationSettings
    { depositFee  = 7_500
    , withdrawFee = 5_000
    , daoShare    = 200_000
    , affShare    = 300_000
    , minDeposit  = 1_000_000
    , minWithdraw = 1_000_000
    , minClaim    = 10_000_000
    }

testStakingClaimSettings :: StakingSettings
testStakingClaimSettings = testStakingSettings
    { opSettings = testOperationSettings { minClaim = 1 }
    }
