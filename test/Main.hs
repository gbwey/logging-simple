{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- staticDhallExpression checks for syntax only! doesnt load into haskell type
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Data.Generics.Labels ()
import Data.Maybe (isNothing)
import Data.Text as T (unlines)
import Dhall
import Logging
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

lg :: LogOpts
lg = $(loadDhallTH @LogOpts "./test_log.dhall")

lgempty :: LogOpts
lgempty = $(loadDhallTH @LogOpts "./test_log_empty.dhall")

lgdef :: IO LogOpts
lgdef = loadFromLogConfig "(./corelog.dhall)::{=}"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  lg1 <- lgdef
  defaultMain $
    testGroup
      "Logging"
      [ testCase "dir in file" $ (@?=) (Just "testdir") (lg ^? #lLogFile . _Just . #fDir)
      , testCase "smtpfrom in email" $ (@?=) (Just "testsmtpfrom") (lg ^? #lEmail . _Just . #eSmtpFrom)
      , testCase "stdout in screen" $ (@?=) (Just StdOut) (lg ^? #lScreen . _Just . #sScreenType)
      , testCase "empty file" $ assertBool "a1" $ isNothing $ lgempty ^. #lLogFile
      , testCase "empty email" $ assertBool "a2" $ isNothing $ lgempty ^. #lEmail
      , testCase "empty screen" $ assertBool "a3" $ isNothing $ lgempty ^. #lScreen
      , testCase "defPrefix" $ (@?=) (Just "def") (lg1 ^? #lLogFile . _Just . #fPrefix)
      , testCase "defDir" $ (@?=) (Just ".") (lg1 ^? #lLogFile . _Just . #fDir)
      , testCase "empty debug false" $ assertBool "a3" $ not (lgempty ^. #lDebug)
      , testCase "debug true" $ assertBool "a3" $ lg ^. #lDebug
      , testCase "todhall: log debug" $ testtodhall1 >>= \f -> f (LogOpts Nothing Nothing Nothing False) @?= (False, Nothing)
      , testCase "todhall: log debug" $ testtodhall1 >>= \f -> f (LogOpts (Just (LogFile "fn" True Info "fp")) Nothing Nothing True) @?= (True, Just (LogFile "fn" True Info "fp"))
      ]

testtodhall1 :: IO (LogOpts -> (Bool, Maybe LogFile))
testtodhall1 =
  let txt =
        T.unlines
          [ "let x = ./corelog.dhall"
          , "in \\(y : x.Type) -> { _1 = y.Debug, _2 = y.LogFile }"
          ]
   in input auto txt
