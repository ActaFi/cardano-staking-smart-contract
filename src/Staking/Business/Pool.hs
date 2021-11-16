{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards    #-}

{-|
Module      : Staking.Business.Pool
Description : Business logic related to registration of users in
              the staking pool.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Staking.Business.Pool where

-- Third-party libraries.
import Ledger
import PlutusTx.Prelude

-- Internal modules.
import Staking.Business.Types


{-# INLINABLE register #-}
register :: PoolState -> PubKeyHash -> AssetClass -> PoolState
register PoolState{..} pkh ac = mkPoolState $ (pkh, ac) : activeUsers

{-# INLINABLE unregister #-}
unregister :: PoolState -> PubKeyHash -> PoolState
unregister PoolState{..} pkh = mkPoolState $ filter ((pkh /=) . fst) activeUsers

{-# INLINABLE isRegistered #-}
isRegistered :: PubKeyHash -> PoolState -> Bool
isRegistered pkh PoolState{..} =
    activeUsers /= filter ((pkh /=) . fst) activeUsers

{-# INLINABLE getUserNFT #-}
getUserNFT :: PoolState -> PubKeyHash -> Maybe AssetClass
getUserNFT PoolState{..} pkh = snd <$> find ((== pkh) . fst) activeUsers

{-# INLINABLE isOneElemExtensionOf #-}
isOneElemExtensionOf :: PoolState -> PoolState -> Bool
isOneElemExtensionOf ps1 ps2 =
       all ((`isRegistered` ps1) . fst) (activeUsers ps2)
    && length (activeUsers ps2) + 1 == length (activeUsers ps1)
