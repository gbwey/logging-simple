-- hlint . --cpp-define=mingw32_HOST_OS
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{- |
Module      : Logging
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3

Mainly has various logging functions and timing of commands.
Allows you to log to a file or the screen or both
-}
module Logging where
import qualified Data.UnixTime as UT
import Data.Time
import System.Clock (getTime, Clock(Monotonic), TimeSpec(sec))
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Control.Monad.Reader (when, MonadIO(..), unless, MonadReader, ReaderT(runReaderT))
import qualified Control.Exception as E
import Control.Monad.Logger
import System.Log.FastLogger
    ( defaultBufSize,
      newFileLoggerSet,
      pushLogStr,
      rmLoggerSet,
      LoggerSet )
import qualified Data.ByteString.Char8 as B
import Formatting (string, (%), formatToString, Format)
import qualified Formatting.Time as F
import System.IO (Handle, stdout, stderr)
import System.Time.Extra (showDuration)
import Network.Mail.SMTP (sendMail, simpleMail)
import Network.Mail.Mime (plainPart)
import System.Environment (getEnvironment)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified UnliftIO as U
import Data.String (IsString(fromString))
import qualified GHC.Generics as G (Generic)
import Data.Maybe (maybe, fromMaybe)
import qualified Language.Haskell.TH.Syntax as TH
import Instances.TH.Lift ()
import Data.Char (toLower)
import qualified Data.List as L
import System.Directory (doesDirectoryExist, canonicalizePath)
import GHC.Stack
#ifdef mingw32_HOST_OS
import qualified System.Win32.Time as W32
import System.Win32.Time (SYSTEMTIME(..))
import Text.Printf (printf)
#endif
import qualified DateTA
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)

{-# COMPLETE GBException #-}
pattern GBException :: HasCallStack => Text -> GBException'
pattern GBException txt <- GBException' txt where
  GBException txt = GBException' (txt <> "\n" <> T.pack (prettyCallStack callStack))

newtype GBException' = GBException' { gbMessage :: Text }
  deriving (G.Generic, Show, Eq)
instance E.Exception GBException'

-- | 'ML' has the minimum set of constraints for running sql commands in sqlhandler-odbc
-- use MonadReader e to customise the environment
type ML e m = (MonadUnliftIO m, MonadLogger m, MonadLoggerIO m, MonadReader e m)
-- | 'RL' defines the outer two layers of 'ML'
type RL e m a = ReaderT e (LoggingT m) a

data LLog =
    Debug
  | Info
  | Warn
  | Error
  deriving (TH.Lift, G.Generic, Show, Eq, Enum, Bounded, Ord)

-- | log to the screen
data ScreenType =
    StdOut
  | StdErr
  deriving (TH.Lift, G.Generic, Show, Eq, Enum, Bounded, Ord)

data Screen = Screen {
      sScreenType :: !ScreenType
    , sLevel :: !LLog
    } deriving (TH.Lift, G.Generic, Show, Eq)

-- | log to a file
data File = File {
      fPrefix :: !FilePath -- ^ basename of log file
    , fLongName :: !Bool -- ^ whether to use a unique name based on datetime or the 'lfName' as is
    , fLevel :: !LLog
    , fDir :: !FilePath
    } deriving (TH.Lift, G.Generic, Show, Eq)

data Email = Email
       { eSmtpServer :: !Text
       , eSmtpTo :: !Text
       , eSmtpFrom :: !Text
       } deriving (TH.Lift, G.Generic, Show)

data LogOpts = LogOpts
       { lFile :: !(Maybe File)
       , lScreen :: !(Maybe Screen)
       , lEmail :: !(Maybe Email)
       , lDebug :: !Bool
       } deriving (TH.Lift, G.Generic, Show)

toLogLevel :: LLog -> LogLevel
toLogLevel =
  \case
     Debug -> LevelDebug
     Info -> LevelInfo
     Warn -> LevelWarn
     Error -> LevelError

chklogdir :: FilePath -> IO ()
chklogdir dir = do
  b <- doesDirectoryExist dir
  unless b $ do
   let msg = "Logging.hs: directory [" <> T.pack dir <> "] does not exist for logging to a file: see key File/Some/Dir"
   T.putStrLn msg
   U.throwIO $ GBException msg

logWith :: MonadUnliftIO m
        => e
        -> LogOpts
        -> RL e m a
        -> m a
logWith = logWith' Nothing

logWith' :: MonadUnliftIO m
         => Maybe UT.UnixTime
         -> e
         -> LogOpts
         -> RL e m a
         -> m a
logWith' = logWithImpl $ fmap toLogStr
#ifdef mingw32_HOST_OS
  ioDateWin32Katip
#else
  ioDateU
#endif

-- | log using the LogOpts and pass in the reader value
--   only sends emails when there is an exception ie logError does not send an email
logWithImpl :: MonadUnliftIO m
            => IO LogStr
            -> Maybe UT.UnixTime
            -> e
            -> LogOpts
            -> RL e m a
            -> m a
logWithImpl mdt mut e opts@LogOpts {lFile, lDebug, lScreen} mra = do
  when lDebug $ liftIO $ T.putStrLn $ "starting in debug mode: " <> T.pack (show opts)
  let ma = runReaderT mra e
  case lFile of
    Just logfn -> do
      let dir = fDir logfn
      liftIO $ chklogdir dir
--      fmt1 <- liftIO $ T.unpack . (if fLongName logfn then formatAsFileNameLong else formatAsFileNameShort) <$> W32.getLocalTime
      fmt1 <- liftIO $ fmap B.unpack (UT.formatUnixTime (if fLongName logfn then fmtLong1 else fmtShort1) =<< Data.Maybe.maybe UT.getUnixTime pure mut)
      fn <- liftIO $ canonicalizePath $ dir <> "/" <> fPrefix logfn <> "_" <> fmt1 <> ".log"
      runMyFileLoggingT mdt (fLevel logfn, lScreen) fn $ emailOnError (T.pack fn) opts ma
    Nothing ->
      let f = case lScreen of
                  Nothing -> flip runLoggingT (\_ _ _ _  -> return ()) -- skip logging entirely
                  Just (Screen ss p) ->
                    toLoggingT ss . filterLogger (\_ lvl -> toLogLevel p <= lvl)
      in f (emailOnError "no file" opts ma)

-- | custom logger for writing to a file
runMyFileLoggingT :: MonadUnliftIO m
                  => IO LogStr
                  -> (LLog, Maybe Screen)
                  -> FilePath
                  -> LoggingT m b
                  -> m b
runMyFileLoggingT mdt p fn logt =
  U.bracket (liftIO $ newFileLoggerSet defaultBufSize fn)
     (liftIO . rmLoggerSet)
     (runLoggingT logt . loggerSetOutput mdt p)

loggerSetOutput :: IO LogStr
              -> (LLog, Maybe Screen)
              -> LoggerSet
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
loggerSetOutput mdt (pfile, mstdout) logt l s level msg = do
  case mstdout of
    Nothing -> return ()
    Just (Screen x pscreen) ->
      when (toLogLevel pscreen <= level) $ B.hPutStrLn (toSout x) (dispLevel level <> B.take 8000 (fromLogStr msg)) -- to avoid overflow to stdout
  when (toLogLevel pfile <= level) $ do
    timestampStr <- mdt
    pushLogStr logt $ defaultLogStr l s level (timestampStr <> " " <> msg)

toLoggingT :: MonadIO m => ScreenType -> (LoggingT m a -> m a)
toLoggingT = \case
  StdOut -> runStdoutLoggingT
  StdErr -> runStderrLoggingT

emailOnError :: (MonadLogger m, MonadUnliftIO m)
              => Text
              -> LogOpts
              -> m a
              -> m a
emailOnError txt opts ma =
  U.catchAny ma $ \x -> do
    $logError $ "outermost error: " <> T.pack (show x)
    liftIO $ sendemail opts txt (T.pack (show x))
    U.throwIO x

sendemail :: LogOpts
          -> Text
          -> Text
          -> IO ()
sendemail LogOpts {lEmail, lDebug} txt exc =
  case lEmail of
     Nothing -> when lDebug $ T.putStrLn $ "debug: no email specified: txt=[" <> txt <> "] exc=[" <> exc <> "]"
     Just email -> do
       when lDebug $ T.putStrLn $ "debug: sending email: txt=[" <> txt <> "] exc=[" <> exc <> "]"
       es <- loadEnvs
       emailMessage email ("failure: " <> txt) [TL.fromStrict exc, es]

{-
ioDateC :: TimeZone -> IO BB.Builder
ioDateC tz = do
  tm <- C.now
  return $ C.builderUtf8_YmdHMSz C.OffsetFormatColonOff C.SubsecondPrecisionAuto C.w3c (C.timeToOffsetDatetime (C.Offset (timeZoneMinutes tz)) tm)
{-# INLINEABLE ioDateC #-}
-}
ioDateU :: IO B.ByteString
ioDateU = do
  tm <- UT.getUnixTime
  UT.formatUnixTime "%FT%T%z" tm
{-# INLINEABLE ioDateU #-}

ioDateD :: IO String
ioDateD = formatTime defaultTimeLocale "%FT%T.%3q%z" <$> getZonedTime
{-# INLINEABLE ioDateD #-}

#ifdef mingw32_HOST_OS
ioDateWin32Katip :: IO Text
ioDateWin32Katip = DateTA.formatAsTimeStampST <$> W32.getLocalTime
{-# INLINEABLE ioDateWin32Katip #-}

ioDateWin32Printf :: IO String
ioDateWin32Printf = do
  SYSTEMTIME{..} <- W32.getLocalTime
  return $ printf "%d-%02d-%02dT%02d:%02d:%02d.%03d" wYear wMonth wDay wHour wMinute wSecond wMilliseconds
{-# INLINEABLE ioDateWin32Printf #-}

#endif

ioDateP :: TimeZone -> IO Text
ioDateP tz = do
  tm <- getPOSIXTime
  let y = utcToZonedTime tz (posixSecondsToUTCTime tm)
  return $ DateTA.formatAsTimeStampP y
{-# INLINEABLE ioDateP #-}

toSout :: ScreenType -> Handle
toSout =
  \case
    StdOut -> stdout
    StdErr -> stderr

fileNameDate :: tm
             -> Format (String -> String) (tm -> String -> String)
             -> String
             -> String
             -> FilePath
fileNameDate tm fmt pref = formatToString (string % "_" % fmt % string) pref tm
{-
fileNameDateQualified :: FormatTime a => a -> String -> String -> String
fileNameDateQualified tm pref = formatToString (string % "_" % fmtLong % string) pref tm
-}
fmtShort1, fmtLong1 :: B.ByteString
fmtShort1 = "%Y%m%d"
fmtLong1 = fmtShort1 <> "_%H%M%S"

fmtShort, fmtLong, fmtLongCrazy :: FormatTime a => Format r (a -> r)
fmtShort = F.year <> F.month <> F.dayOfMonth
fmtLong = fmtShort <> "_" % F.hour24 <> F.minute <> F.second
fmtLongCrazy = fmtLong <> "." % F.pico

loadEnvs :: IO TL.Text
loadEnvs =
    TL.pack
  . unlines
  . map (\(x,y) -> x <> " = " <> y)
  . filter (\(x,_) -> let z = map toLower x
                      in not (L.isInfixOf "pwd" z || L.isInfixOf "pass" z)
           )
  <$> getEnvironment

-- | send an email using 'myemail' which pulls the settings from log.dhall
emailMessage :: Email -> Text -> [TL.Text] -> IO ()
emailMessage email subj bodys =
  sendMail (T.unpack (eSmtpServer email))
          $ simpleMail
              (fromString (T.unpack (eSmtpFrom email)))
              [fromString (T.unpack (eSmtpTo email))]
              []
              []
              subj
              [plainPart $ TL.intercalate "\n\n" bodys]

-- | used for logging start and end time of a job
timeCommand :: ML e m => Text -> m a -> m a
timeCommand = timeCommand' const

timeCommand' :: ML e m
             => (a -> (UT.UnixTime, UT.UnixTime) -> b)
             -> Text
             -> m a
             -> m b
timeCommand' callback txt cmd = do
  (c,c1,a) <- do
    c <- liftIO UT.getUnixTime
    c1 <- liftIO $ T.decodeUtf8 <$> fmtZt1 c
    let msg = "Start TimeCommand " <> c1 <> " " <> txt
    $logInfo msg
    a <- liftIO $ getTime Monotonic
    return (c,c1,a)
  ret <- U.try @_ @E.SomeException $ cmd >>= \x -> return $! x
  do
    b <- liftIO $ getTime Monotonic
    d <- liftIO UT.getUnixTime
    d1 <- liftIO $ T.decodeUtf8 <$> fmtZt1 d
    let xs = difftimes a b <> " started=" <> c1 <> " ended=" <> d1
    case ret of
      Left e -> do
                  let msg = "FAILURE!!!! TimeCommand " <> xs <> " " <> txt <> " [" <> T.pack (show e) <> "]"
                  $logError msg
                  U.throwIO e
      Right x -> do
                   $logInfo $ "OK TimeCommand " <> xs <> " " <> txt
                   return $ callback x (c,d)

difftimes :: TimeSpec -> TimeSpec -> Text
difftimes a b = T.pack $ showDuration (fromIntegral (sec (b - a)))

fmtZt :: ZonedTime -> String
fmtZt =  formatTime defaultTimeLocale "%T"

fmtZt1 :: UT.UnixTime -> IO B.ByteString
fmtZt1 =  UT.formatUnixTime "%T"

dispLevel :: LogLevel -> B.ByteString
dispLevel LevelDebug = mempty
dispLevel LevelInfo = mempty
dispLevel LevelWarn = "WARN: "
dispLevel LevelError = "ERROR: "
dispLevel (LevelOther txt) = T.encodeUtf8 txt <> ": "

-- | MyLogger is a manual logger when you dont have access to MonadLogger
type MyLogger = LogLevel -> Text -> IO ()

getLogger :: MonadLoggerIO m => Loc -> m MyLogger
getLogger loc = do
  x <- askLoggerIO
--  return (\lvl msg -> x (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) "" lvl (toLogStr msg))
  return (\lvl msg -> x loc "" lvl (toLogStr msg))

lgDebug, lgInfo, lgWarn, lgError :: MonadIO m => MyLogger -> Text -> m ()
lgDebug lg = liftIO . lg LevelDebug
lgInfo lg = liftIO . lg LevelInfo
lgWarn lg = liftIO . lg LevelWarn
lgError lg = liftIO . lg LevelError

fs :: MonadUnliftIO m => RL () m a -> m a
fs = fse ()

fse :: MonadUnliftIO m => e -> RL e m a -> m a
fse e = logWith e logs

fnone :: MonadUnliftIO m => RL () m a -> m a
fnone = logWith () lognone

-- basic ones that i use all the time
-- no need to read the dhall files for these
fd, fi, fw :: MonadUnliftIO m => RL () m a -> m a
fd = fde ()
fi = fie ()
fw = fwe ()

fde, fie, fwe :: MonadUnliftIO m => e -> RL e m a -> m a
fde e = logWith e logd
fie e = logWith e (logx Info)
fwe e = logWith e (logx Warn)

setFileLong :: Bool -> LogOpts -> LogOpts
setFileLong b ll =
  ll { lFile = (\x -> x { fLongName = b }) <$> lFile ll }

lognone :: LogOpts
lognone = LogOpts Nothing
               Nothing
               Nothing
               False

logs :: LogOpts
logs = LogOpts Nothing
               (Just (Screen StdOut Debug))
               Nothing
               False

logd :: LogOpts
logd = logx Debug

logx :: LLog -> LogOpts
logx lvl = LogOpts (Just (File "def" True Debug "."))
               (Just (Screen StdOut lvl))
               Nothing
               False

chk :: LLog
    -> Maybe LLog
    -> LogOpts
    -> LogOpts
chk deflvl mlvl opts = opts { lScreen = Just (Screen StdOut (fromMaybe deflvl mlvl)) }

newline :: Text
newline = "\n"


