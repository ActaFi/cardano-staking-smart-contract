{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ExistentialQuantification #-}

module BCExplorer where

{-|
Module      : BCExplorer
Description : Blockchain explorer for traces.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

import qualified Prelude                    as HP
import           System.IO                  (Handle, hPutStrLn)
import           System.Console.ANSI

import           Control.Lens               hiding (index, under)
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer
import           Control.Monad.Freer.Extras.Log
import           Control.Monad.Freer.TH         (makeEffect)
import           Control.Monad.IO.Class              (MonadIO (..))

import           Data.Default
import qualified Data.Set                   as Set
import qualified Data.Map                   as Map
import           Data.Maybe                 ( fromJust )

import           Ledger
import           Ledger.Value               as Value
import           Plutus.Trace.Emulator      ( chainNewestFirst
                                            , EmulatorTrace
                                            , TraceConfig
                                            , EmulatorConfig
                                            )
import qualified Plutus.Trace.Emulator      as Emulator
import qualified Plutus.PAB.Simulator                as Simulator

import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator.Wallet
import           Wallet.Emulator.MultiAgent ( EmulatorState (..)
                                            , _eteEmulatorTime
                                            , _eteEvent
                                            )

import           Plutus.V1.Ledger.Api
import           Ledger.TimeSlot

data FromDataFunc = forall a. (ToData a, HP.Show a) => FD (Data -> Maybe a)

makeEffect ''Emulator.PrintEffect

traceNFTFD :: [FromDataFunc] -> AssetClass -> Simulator.Simulation t ()
traceNFTFD fds nft = Simulator.blockchain >>= findPrintLastTx
  where
    findPrintLastTx :: Blockchain -> Simulator.Simulation t ()
    findPrintLastTx bss = Control.Monad.mapM_ (pp bss) $ HP.reverse bss

    pp :: Blockchain -> [OnChainTx] -> Simulator.Simulation t ()
    pp bss ontxs = case filter nftTx ontxs of
                     [] -> return ()
                     bs -> liftIO $ HP.putStrLn $ printTxs fds bss bs

    nftTx :: OnChainTx -> Bool
    nftTx = any (checkTxHasNFT nft) . txOutputs . getTx

    checkTxHasNFT :: AssetClass -> TxOut -> Bool
    checkTxHasNFT asc o = assetClassValueOf (txOutValue o) asc == 1

printPABLastTx :: Simulator.Simulation t ()
printPABLastTx = printPABLastTxFD []

printPABLastTxFD :: [FromDataFunc] -> Simulator.Simulation t ()
printPABLastTxFD fds = Simulator.blockchain >>= findPrintLastTx
  where
    findPrintLastTx :: Blockchain -> Simulator.Simulation t ()
    findPrintLastTx []       = return ()
    findPrintLastTx ([]:bss) = findPrintLastTx bss
    findPrintLastTx (bs:bss) = liftIO $ HP.putStrLn $ printTxs fds bss $
                                                      take 1 bs

printPABLastTxAtSlot :: Integer -> Simulator.Simulation t ()
printPABLastTxAtSlot n = Simulator.blockchain >>= findPrintLastTx
  where
    findPrintLastTx :: Blockchain -> Simulator.Simulation t ()
    findPrintLastTx bss | n < length bss =
                            liftIO $ HP.putStrLn $
                            printTxs [] bss $
                            take 1 (bss !! (length bss HP.- n HP.- 1))
                        | otherwise = return ()

printBlockChainCFD :: [FromDataFunc] -> Emulator.EmulatorTrace ()
printBlockChainCFD fds = Emulator.chainState >>= \chain -> do
  case chain ^. chainNewestFirst of
    []       -> return ()
    bss -> Control.Monad.mapM_ (logInfo @HP.String . printTxs fds bss) $
                               HP.reverse bss

printLastTx :: Emulator.EmulatorTrace ()
printLastTx = printLastTxFD []

printLastTxFD :: [FromDataFunc] -> Emulator.EmulatorTrace ()
printLastTxFD fds = Emulator.chainState >>= \chain -> do
  case chain ^. chainNewestFirst of
    []       -> return ()
    (bs : bss) -> logInfo @HP.String $ printTxs fds bss bs

lookUpUnspendOut :: Blockchain -> TxOutRef -> Maybe (Tx, TxOut)
lookUpUnspendOut bss oref =
  case concatMap (mapMaybe (lkOutTx . getTx)) bss of
    [o] -> Just o
    _   -> Nothing
  where
    lkOutTx :: Tx -> Maybe (Tx, TxOut)
    lkOutTx tx = (tx,) <$> Map.lookup oref (unspentOutputsTx tx)

getTx :: OnChainTx -> Tx
getTx (Invalid tx) = tx
getTx (Valid tx)   = tx

printTxs :: [FromDataFunc] -> Blockchain -> [OnChainTx] -> HP.String
printTxs fds bss = HP.unlines . map (printTx fds bss)

printTx :: [FromDataFunc] -> Blockchain -> OnChainTx -> HP.String
printTx fds bss ontx =
  HP.unlines [ ""
             , txType
             , HP.unwords
               [ under $ bold "POSIXTime range:"
               , HP.show (slotRangeToPOSIXTimeRange def $ txValidRange tx)
               ]
             , HP.unwords
               [ under $ bold "Slot range:"
               , HP.show (txValidRange tx)
               ]
             , "┏━" ++ bold " Inputs "  ++ HP.replicate 80 '━'
             , HP.unlines (map (printInput fds bss) inps)
             , "┏━" ++ bold " Outputs " ++ HP.replicate 80 '━'
             , HP.unlines (map (printOut fds tx) outs)
             ]
  where
    tx = getTx ontx
    txType = case ontx of
               Valid _   -> bold "Valid Transaction"
               Invalid _ -> bold "Invalid Transaction"
    inps = Set.toList $ txInputs tx
    outs = zip [0..] $ txOutputs tx

printInput :: [FromDataFunc] -> Blockchain -> TxIn -> HP.String
printInput fds bss txin =
  HP.unlines $ map ("┃ " ++)
  [ bold "<< In"
  , HP.unwords [under "UTxORef", "|", HP.show $ txInRef txin]
  , HP.unwords [under "Type", "   |", ppTxInType $ txInType txin]
  , HP.unwords [under "Value", "  |", v]
  , HP.unwords [under "Datum", "  |", ppDatum md]
  , bold ">>"
  ]
  where
    ppTxInType :: Maybe TxInType -> HP.String
    ppTxInType Nothing = "Nothing"
    ppTxInType (Just (ConsumeScriptAddress csv csr csd)) =
      HP.unwords [ "ConsumeScriptAddress"
                 , HP.show csv
                 , HP.show csr
                 , ppDatum $ Just csd
                 ]
    ppTxInType (Just ConsumePublicKeyAddress) = "ConsumePublicKeyAddress"

    txout :: Maybe (Tx, TxOut)
    txout = lookUpUnspendOut bss $ txInRef txin

    v :: HP.String
    v = maybe "" (HP.show . txOutValue . snd) txout

    ppDatum :: Maybe Datum -> HP.String
    ppDatum md' =
      case (md', find (\(FD f) -> isJust $ f $
                                  toData $ getDatum (fromJust md')) fds) of
        (Nothing, _)           -> "Nothing"
        (Just d', Nothing)     -> HP.show d'
        (Just d', Just (FD f)) -> HP.show $ f $ toData $ getDatum d'

    md :: Maybe Datum
    md = txout >>= \(tx, o) -> txOutDatumHash o >>= flip Map.lookup (txData tx)

printOut :: [FromDataFunc] -> Tx -> (Integer,TxOut) -> HP.String
printOut fds tx (idx,txout) =
  HP.unlines $ map ("┃ " ++)
  [ bold "<< Out"
  , HP.unwords [under "UTxORef", "|", HP.show $ TxOutRef (txId tx) idx]
  , HP.unwords [under "Address", "|", addr]
  , HP.unwords [under "Value", "  |", v]
  , HP.unwords [under "Datum", "  |", d]
  , bold ">>"
  ]
  where
    addr :: HP.String
    addr = HP.show $ txOutAddress txout
    v :: HP.String
    v = HP.show $ txOutValue txout

    d :: HP.String
    d = case (md, find (\(FD f) -> isJust $ f $
                                   toData $ getDatum (fromJust md)) fds) of
          (Nothing, _)           -> "Nothing"
          (Just d', Nothing)     -> HP.show d'
          (Just d', Just (FD f)) -> HP.show $ f $ toData $ getDatum d'

    md :: Maybe Datum
    md = txOutDatumHash txout >>= flip Map.lookup (txData tx)

bold :: HP.String -> HP.String
bold str = setSGRCode [SetConsoleIntensity BoldIntensity]
           ++
           str
           ++
           setSGRCode [SetConsoleIntensity NormalIntensity]

under :: HP.String -> HP.String
under str = setSGRCode [SetUnderlining SingleUnderline]
            ++
            str
            ++
            setSGRCode [SetUnderlining NoUnderline]

color :: Color -> Emulator.EmulatorTrace a -> Emulator.EmulatorTrace a
color c m = do
  logError @HP.String $ setSGRCode [SetColor Foreground Vivid c]
  a <- m
  logError @HP.String $ setSGRCode [SetColor Foreground Vivid White]
  return a

pad :: HP.Int -> Integer -> HP.String
pad n = (\x -> HP.replicate (n HP.- HP.length x) '0' ++ x) . HP.show

printBalances :: forall effs. Member Emulator.PrintEffect effs
              => Map.Map Entity Value
              -> Eff effs ()
printBalances m = do
    forM_ (Map.toList m) $ \(e, v) -> do
        printLn $ HP.show e <> ": "
        forM_ (flattenValue v) $ \(cs, tn, a) ->
            printLn $ "    {" <> HP.show cs <> ", " <>
                                 HP.show tn <> "}: " <> HP.show a

runEmulatorTraceEff
  :: forall effs. Member Emulator.PrintEffect effs
  => TraceConfig
  -> EmulatorConfig
  -> EmulatorTrace ()
  -> Eff effs ()
runEmulatorTraceEff tcfg cfg t =
  let (xs, me, e) = Emulator.runEmulatorTrace cfg t
      balances' = balances (_chainState e) (_walletStates e)
   in do
      case me of
        Nothing  -> return ()
        Just err -> printLn $ "ERROR: " <> HP.show err

      forM_ xs $ \ete -> do
        case Emulator.showEvent tcfg (_eteEvent ete) of
          Nothing -> return ()
          Just s  ->
            let slot = pad 5 (getSlot $ _eteEmulatorTime ete)
             in printLn ("Slot " <> slot <> ": ") >>
                Control.Monad.mapM_ printLn (HP.lines s)

      printLn "Final balances"
      printBalances balances'

runPrintEffect
  :: Handle
  -> Eff '[Emulator.PrintEffect, HP.IO] r
  -> HP.IO r
runPrintEffect hdl = runM . interpretM f
  where
    f :: Emulator.PrintEffect r -> HP.IO r
    f = \case
      Emulator.PrintLn s -> hPutStrLn hdl s

runEmulatorTraceIO'
  :: TraceConfig
  -> EmulatorConfig
  -> EmulatorTrace ()
  -> HP.IO ()
runEmulatorTraceIO' tcfg cfg t
  = runPrintEffect (Emulator.outputHandle tcfg) $
    runEmulatorTraceEff tcfg cfg t
