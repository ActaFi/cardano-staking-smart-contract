{-|
Module      : Main
Description : The main tests file that calls every test with one command.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Main ( main ) where

import Test.Tasty (defaultMain)

import Tests.Tests (tests)

main :: IO ()
main = defaultMain tests
