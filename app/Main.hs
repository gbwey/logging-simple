{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Logger
import Criterion.Main
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Data.UnixTime as UT
import qualified DateTA
import Logging

main :: IO ()
main = do
  tz <- getCurrentTimeZone
  defaultMain
    [ bgroup
        "log with times"
        [ logTime tz "small" 100
        , logTime tz "medium" 1000
        , logTime tz "large" 10000
        ]
    , bgroup
        "times only"
        [ --         bench "chronos" $ nfIO $ BL.toStrict . BB.toLazyByteString <$> ioDateC tz
          bench "unix-time" $ nfIO ioDateU
        , bench "data.time" $ nfIO $ B8.pack <$> ioDateD
        , bench "posix-katip" $ nfIO $ T.encodeUtf8 <$> DateTA.ioDatePosixKatip tz
#ifdef mingw32_HOST_OS
       , bench "win32-katip" $ nfIO $ T.encodeUtf8 <$> ioDateWin32Katip
       , bench "win32-printf" $ nfIO $ B8.pack <$> ioDateWin32Printf
#endif
        ]
    ]

logTime :: TimeZone -> String -> Int -> Benchmark
logTime !tz !desc !sz =
  bgroup
    desc
    [ --      bench "chronos" $ nfIO $ runChronos sz tz
      bench "unix-time" $ nfIO $ runUnixTime sz
    , bench "data.time" $ nfIO $ runDataTime sz
    , bench "posix-katip" $ nfIO $ runPosixKatip sz tz
#ifdef mingw32_HOST_OS
    , bench "win32-katip" $ nfIO $ runWin32Katip sz
    , bench "win32-printf" $ nfIO $ runWin32Printf sz
#endif
    ]

runit :: Int -> (Maybe UT.UnixTime -> () -> LogOpts -> RL () IO () -> IO ()) -> String -> IO ()
runit sz fn nm =
  fn Nothing () (LogOpts (Just (LogFile nm True Debug ".")) Nothing Nothing False) $
    forM_ [1 :: Int .. sz] $ \i -> $logDebug (T.pack (show i) <> "hello world this is a test")

--runChronos :: Int -> TimeZone -> IO ()
--runChronos !sz !tz = runit sz (logWithImpl (toLogStr <$> ioDateC tz)) "tst_chronos"

runUnixTime :: Int -> IO ()
runUnixTime !sz = runit sz (logWithImpl (toLogStr <$> ioDateU)) "tst_unixtime"

runDataTime :: Int -> IO ()
runDataTime !sz = runit sz (logWithImpl (toLogStr <$> ioDateD)) "tst_datatime"

#ifdef mingw32_HOST_OS
runWin32Printf :: Int -> IO ()
runWin32Printf sz = runit sz (logWithImpl (toLogStr <$> ioDateWin32Printf)) "tst_win32printf"

runWin32Katip :: Int -> IO ()
runWin32Katip !sz = runit sz (logWithImpl (toLogStr <$> ioDateWin32Katip)) "tst_win32katip"

#endif

runPosixKatip :: Int -> TimeZone -> IO ()
runPosixKatip sz tz = runit sz (logWithImpl (toLogStr <$> ioDateP tz)) "tst_posixkatip"
