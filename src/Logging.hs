-- make the email one plainPart else stuff can be dropped in the email! has happened
-- haskellnet is no longer updated so dump it
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Logging
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3

Mainly has various logging functions and timing of commands.
Allows you to log to a file or the screen or both
-}
module Logging (
  module Logging,
  module Control.Monad.Logger,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B8
import Data.Char
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time
import qualified Data.UnixTime as UT
import qualified GHC.Generics as G (Generic)
import GHC.Stack
import Instances.TH.Lift ()
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.Mail.Mime as NMM
import qualified Network.Mail.SMTP as OLD
import System.Clock
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.Environment (getEnvironment)
import System.IO (Handle, hFlush, stderr, stdout)
import System.Log.FastLogger (
  LoggerSet,
  defaultBufSize,
  newFileLoggerSet,
  pushLogStr,
  rmLoggerSet,
 )
import qualified UnliftIO as U

#ifdef mingw32_HOST_OS
import qualified System.Win32.Time as W32
import System.Win32.Time (SYSTEMTIME(..))
import Text.Printf (printf)
#endif

import BaseUtils.Extra
import Data.Bool (bool)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified DateTA
import Dhall (
  Decoder,
  FromDhall (..),
  ToDhall (..),
  genericAutoWith,
  genericToDhallWith,
  input,
 )
import qualified Dhall as D
import DocUtils.Doc
import DocUtils.Time
import GHC.Natural
import Prettyprinter
import Primus.Error (normalError)
import Primus.Extra ((.@))

{- | 'ML' has the minimum set of constraints for running sql commands in sqlhandler-odbc
 use MonadReader e to customise the environment
type ML e m = (MonadUnliftIO m, MonadLogger m, MonadLoggerIO m, MonadReader e m)
-}
type ML e m = (HasCallStack, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m, MonadReader e m)

-- | 'RL' defines the outer two layers of 'ML'
type RL e m a = ReaderT e (LoggingT m) a

-- | log verbose settings
data LLog
  = Debug
  | Info
  | Warn
  | Error
  deriving stock (TH.Lift, G.Generic, Show, Eq, Enum, Bounded, Ord)

-- | log to the screen options
data ScreenType
  = StdOut
  | StdErr
  deriving stock (TH.Lift, G.Generic, Show, Eq, Enum, Bounded, Ord)

-- | log to the screen
data Screen = Screen
  { sScreenType :: !ScreenType
  , sLevel :: !LLog
  }
  deriving stock (TH.Lift, G.Generic, Show, Eq)

-- | log to a file
data LogFile = LogFile
  { fPrefix :: !FilePath
  -- ^ basename of log file
  , fLongName :: !Bool
  , fLevel :: !LLog
  , fDir :: !FilePath
  }
  deriving stock (TH.Lift, G.Generic, Show, Eq)

-- | smtp server settings
data Email = Email
  { eSmtpServer :: !Text
  , eSmtpPort :: !(Maybe Natural)
  , eSmtpTo :: !Text
  , eSmtpFrom :: !Text
  }
  deriving stock (TH.Lift, G.Generic, Show)

-- | log options: ie where to log and how verbose
data LogOpts = LogOpts
  { lLogFile :: !(Maybe LogFile)
  , lScreen :: !(Maybe Screen)
  , lEmail :: !(Maybe Email)
  , lDebug :: !Bool
  }
  deriving stock (TH.Lift, G.Generic, Show)

-- | converts 'LLog' to that of monad-logger
toLogLevel :: LLog -> LogLevel
toLogLevel =
  \case
    Debug -> LevelDebug
    Info -> LevelInfo
    Warn -> LevelWarn
    Error -> LevelError

-- | check that logging directory exists
chklogdir :: FilePath -> IO ()
chklogdir dir = do
  b <- doesDirectoryExist dir
  unless b $ do
    let msg = "Logging.hs: directory [" <> T.pack dir <> "] does not exist for logging to a file: see key LogFile/Some/Dir"
    T.putStrLn msg
    U.throwIO $ GBException msg

-- | log with options but allow system to allocate a logfile if needed
logWith ::
  (HasCallStack, MonadUnliftIO m) =>
  e ->
  LogOpts ->
  RL e m a ->
  m a
logWith = logWith' Nothing

-- | log with options but user to specify a logfile
logWith' ::
  (HasCallStack, MonadUnliftIO m) =>
  Maybe UT.UnixTime ->
  e ->
  LogOpts ->
  RL e m a ->
  m a
logWith' = logWithImpl $ fmap toLogStr

#ifdef mingw32_HOST_OS
  ioDateWin32Katip
#else
  ioDateU
#endif

-- | gets a logfile name using the path and optionally the datetime using "mut" or default to using the current time
getLogName :: LogFile -> Maybe UT.UnixTime -> IO FilePath
getLogName logfn mut = do
  ut <- maybe UT.getUnixTime pure mut
  let dir = fDir logfn
  chklogdir dir
  fmt1 <- B8.unpack <$> UT.formatUnixTime (if fLongName logfn then _YYYYMMDD_HHMMSS else _YYYYMMDD) ut
  canonicalizePath $ dir <> "/" <> fPrefix logfn <> "_" <> fmt1 <> ".log"

{- | log using the LogOpts and pass in the reader value
   only sends emails when there is an exception ie logError does not send an email
-}
logWithImpl ::
  (HasCallStack, MonadUnliftIO m) =>
  IO LogStr ->
  Maybe UT.UnixTime ->
  e ->
  LogOpts ->
  RL e m a ->
  m a
logWithImpl mdt mut e opts mra = do
  when (lDebug opts) $ liftIO $ T.putStrLn $ "starting in debug mode: " <> T.pack (show opts)
  let ma = runReaderT mra e
  case lLogFile opts of
    Just logfn -> do
      fn <- liftIO $ getLogName logfn mut
      runMyFileLoggingT mdt (fLevel logfn, lScreen opts) fn $ emailOnError (T.pack fn) opts ma
    Nothing ->
      let f = case lScreen opts of
            Nothing -> flip runLoggingT (\_ _ _ _ -> return ()) -- skip logging entirely
            Just (Screen ss p) ->
              toLoggingT ss . filterLogger (\_ lvl -> toLogLevel p <= lvl)
       in f (emailOnError "no file" opts ma)

-- | custom logger for writing to a file
runMyFileLoggingT ::
  MonadUnliftIO m =>
  IO LogStr ->
  (LLog, Maybe Screen) ->
  FilePath ->
  LoggingT m b ->
  m b
runMyFileLoggingT mdt p fn logt =
  U.bracket
    (liftIO $ newFileLoggerSet defaultBufSize fn)
    (liftIO . rmLoggerSet)
    (runLoggingT logt . loggerSetOutput mdt p)

-- | outputs a log entry to the screen
loggerSetOutput ::
  IO LogStr ->
  (LLog, Maybe Screen) ->
  LoggerSet ->
  Loc ->
  LogSource ->
  LogLevel ->
  LogStr ->
  IO ()
loggerSetOutput mdt (pfile, mstdout) logt l s level msg = do
  case mstdout of
    Nothing -> return ()
    Just (Screen x pscreen) ->
      when (toLogLevel pscreen <= level) $ B8.hPutStrLn (toSout x) (dispLevel level <> B8.take 8000 (fromLogStr msg)) -- to avoid overflow to stdout
  when (toLogLevel pfile <= level) $ do
    timestampStr <- mdt
    pushLogStr logt $ defaultLogStr l s level (timestampStr <> " " <> msg)

-- | outputs a log entry to stdout or stderr
toLoggingT :: MonadIO m => ScreenType -> (LoggingT m a -> m a)
toLoggingT = \case
  StdOut -> runStdoutLoggingT
  StdErr -> runStderrLoggingT

-- | email the log entry
emailOnError ::
  (HasCallStack, MonadLogger m, MonadUnliftIO m) =>
  Text ->
  LogOpts ->
  m a ->
  m a
emailOnError txt opts ma =
  U.catchAny ma $ \e -> do
    let emsg = "outermost error: " <> psi e
    werrLS $ TL.toStrict emsg
    let (b, e') = innerMostGBWrapException e
    let emsg' = bool "no innermost exception!" ("innermost exception: " <> psi e') b
    liftIO $
      sendemail
        opts
        (T.take 50 txt)
        [ "txt: " <> TL.fromStrict txt
        , emsg'
        , emsg
        ]
    rethrow "emailOnError" e

-- | send an email
sendemail ::
  LogOpts ->
  Text ->
  [TL.Text] ->
  IO ()
sendemail opts subj tls =
  let extra = "subj=[" <> subj <> "]\n[ " <> TL.toStrict (TL.intercalate "\n" tls) <> "\n]"
   in case lEmail opts of
        Nothing -> when (lDebug opts) $ T.putStrLn $ "debug: no email specified: " <> extra
        Just email -> do
          when (lDebug opts) $ T.putStrLn $ "debug: sending email: " <> extra
          es <- loadEnvs
          emailMessage email ("failure:" <> subj) (tls <> [es])

{-
ioDateC :: TimeZone -> IO BB.Builder
ioDateC tz = do
  tm <- C.now
  return $ C.builderUtf8_YmdHMSz C.OffsetFormatColonOff C.SubsecondPrecisionAuto C.w3c (C.timeToOffsetDatetime (C.Offset (timeZoneMinutes tz)) tm)
{-# INLINEABLE ioDateC #-}
-}

-- | get the current datetime as a string for unix
ioDateU :: IO B8.ByteString
ioDateU = do
  tm <- UT.getUnixTime
  UT.formatUnixTime "%FT%T%z" tm
{-# INLINEABLE ioDateU #-}

-- | pretty print current date and timezone
ioDateD :: IO String
ioDateD = formatTime defaultTimeLocale "%FT%T.%3q%z" <$> getZonedTime
{-# INLINEABLE ioDateD #-}

#ifdef mingw32_HOST_OS
-- | get the current datetime as a string for windows
ioDateWin32Katip :: IO Text
ioDateWin32Katip = DateTA.formatAsTimeStampST <$> W32.getLocalTime
{-# INLINEABLE ioDateWin32Katip #-}

-- | pretty print the datetime for windows
ioDateWin32Printf :: IO String
ioDateWin32Printf = do
  SYSTEMTIME{..} <- W32.getLocalTime
  return $ printf "%d-%02d-%02dT%02d:%02d:%02d.%03d" wYear wMonth wDay wHour wMinute wSecond wMilliseconds
{-# INLINEABLE ioDateWin32Printf #-}

#endif

-- | pretty print the datetime with the timezone
ioDateP :: TimeZone -> IO Text
ioDateP tz = do
  tm <- getPOSIXTime
  let y = utcToZonedTime tz (posixSecondsToUTCTime tm)
  return $ DateTA.formatAsTimeStampP y
{-# INLINEABLE ioDateP #-}

-- | choose output handle based on 'ScreenType'
toSout :: ScreenType -> Handle
toSout =
  \case
    StdOut -> stdout
    StdErr -> stderr

-- | dump the environment excluding password related entries
loadEnvs :: IO TL.Text
loadEnvs =
  TL.pack
    . unlines
    . map (\(x, y) -> x <> " = " <> y)
    . filter
      ( \(x, _) ->
          let z = map toLower x
           in not (L.isInfixOf "pwd" z || L.isInfixOf "pass" z)
      )
    <$> getEnvironment

-- | send an email using 'Email' settings
emailMessage :: Email -> Text -> [TL.Text] -> IO ()
emailMessage email subj bodys =
  OLD.sendMail' (T.unpack (eSmtpServer email)) (fromIntegral (fromMaybe 25 (eSmtpPort email))) $
    OLD.simpleMail
      (fromString (T.unpack (eSmtpFrom email)))
      [fromString (T.unpack (eSmtpTo email))]
      []
      []
      subj
      [NMM.plainPart $ TL.intercalate "\n\n" (bodys <> ["subject:" <> TL.fromStrict subj])]

{-
emailMessageNH :: Email -> Text -> [TL.Text] -> IO ()
emailMessageNH email subject bodys =
  NH.doSMTPPort (T.unpack (eSmtpServer email)) (fromIntegral (fromMaybe 25 (eSmtpPort email))) $ \s ->
    flip NH.sendMail s $
      simpleMail
        (fromString (T.unpack (eSmtpFrom email)))
        [fromString (T.unpack (eSmtpTo email))]
        []
        []
        subject
        [NMM.plainPart $ TL.intercalate "\n\n" (bodys <> ["subject:" <> TL.fromStrict subject])]

simpleMail ::
  -- | from
  NMM.Address ->
  -- | to
  [NMM.Address] ->
  -- | CC
  [NMM.Address] ->
  -- | BCC
  [NMM.Address] ->
  -- | subject
  Text ->
  -- | list of parts (list your preferred part last)
  [NMM.Part] ->
  NMM.Mail
simpleMail from to cc bcc subject parts =
  NMM.Mail
    { mailFrom = from
    , mailTo = to
    , mailCc = cc
    , mailBcc = bcc
    , mailHeaders = [("Subject", subject)]
    , mailParts = [parts]
    }
-}

-- | used for logging start and end time of a job but ignore the returned duration
timeCommand :: ML e m => Text -> m a -> m a
timeCommand = fmap fst .@ timeCommand'

-- | used for logging start and end time of a job
timeCommandDiff :: ML e m => Text -> m a -> m (a, Text)
timeCommandDiff = timeCommand'

-- | used for logging start and end time of a job but formatted as an inline 'Doc'
timeCommandDocI :: ML e m => Text -> m [Doc ann] -> m (Doc ann)
timeCommandDocI msg ma = snd <$> timeCommandDoc' True msg (((),) <$> ma)

-- | used for logging start and end time of a job but formatted as a 'Doc'
timeCommandDocNI :: ML e m => Text -> m [Doc ann] -> m (Doc ann)
timeCommandDocNI msg ma = snd <$> timeCommandDoc' False msg (((),) <$> ma)

-- | used for logging start and end time of a job as a 'Doc' with the duration included
timeCommandDoc' :: ML e m => Bool -> Text -> m (a, [Doc ann]) -> m (a, Doc ann)
timeCommandDoc' inline msg ma = do
  ((a, docs), difftm) <- timeCommandDiff msg ma
  return (a, doc1' inline (pretty difftm <+> pretty msg) docs)

-- dont use DiffTimer here cos we want this as fast and minimal as possible
-- not using Monotonic here cos we dont care that much

{- | used for logging start and end time of a job
 | internal command for running a job and keeping track of the time taken
-}
timeCommand' ::
  ML e m =>
  Text ->
  m a ->
  m (a, Text)
timeCommand' txt cmd = do
  let fmtZt1 = UT.formatUnixTime "%T"
  (c1, a) <- do
    c <- liftIO UT.getUnixTime
    c1 <- liftIO $ T.decodeUtf8 <$> fmtZt1 c
    let msg = "Start TimeCommand " <> c1 <> " " <> txt
    $logInfo msg
    a <- liftIO $ getTime Monotonic
    return (c1, a)
  ret <- U.try @_ @U.SomeException $ cmd >>= \x -> return $! x
  do
    (xs, difftm) <- liftIO $ do
      b <- getTime Monotonic
      d <- UT.getUnixTime
      d1 <- T.decodeUtf8 <$> fmtZt1 d
      let difftm = timeSpecDuration (b - a)
      let xs = difftm <> " started=" <> c1 <> " ended=" <> d1
      return (xs, difftm)
    case ret of
      Left e -> do
        werrLS $ "FAILURE!!!! TimeCommand " <> xs <> " " <> txt <> " [" <> psiT e <> "]"
        rethrow ("TimeCommand " <> txt) e
      Right x -> do
        $logInfo $ "OK TimeCommand " <> xs <> " " <> txt
        return (x, difftm <> " " <> c1)

-- | display loglevel as a string
dispLevel :: LogLevel -> B8.ByteString
dispLevel LevelDebug = mempty
dispLevel LevelInfo = mempty
dispLevel LevelWarn = "WARN: "
dispLevel LevelError = "ERROR: "
dispLevel (LevelOther txt) = T.encodeUtf8 txt <> ": "

-- | MyLogger is a manual logger when you dont have access to MonadLogger
type MyLogger = LogLevel -> Text -> IO ()

-- | extract the logger from the environment
getLogger :: MonadLoggerIO m => Loc -> m MyLogger
getLogger loc = do
  x <- askLoggerIO
  --  return (\lvl msg -> x (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) "" lvl (toLogStr msg))
  return (\lvl msg -> x loc "" lvl (toLogStr msg))

-- | debug log
lgDebug :: MonadIO m => MyLogger -> Text -> m ()
lgDebug lg = liftIO . lg LevelDebug

-- | info log
lgInfo :: MonadIO m => MyLogger -> Text -> m ()
lgInfo lg = liftIO . lg LevelInfo

-- | warn log
lgWarn :: MonadIO m => MyLogger -> Text -> m ()
lgWarn lg = liftIO . lg LevelWarn

-- | error log
lgError :: MonadIO m => MyLogger -> Text -> m ()
lgError lg = liftIO . lg LevelError

-- | log to the screen without an environment
fs :: (HasCallStack, MonadUnliftIO m) => RL () m a -> m a
fs = fse ()

-- | log to the screen with an environment
fse :: (HasCallStack, MonadUnliftIO m) => e -> RL e m a -> m a
fse e = logWith e logs

-- | skip logging
fnone :: (HasCallStack, MonadUnliftIO m) => RL () m a -> m a
fnone = logWith () lognone

-- basic ones that i use all the time
-- no need to read the dhall files for these

-- | debug logging to a file without an environment
fd :: (HasCallStack, MonadUnliftIO m) => RL () m a -> m a
fd = fde ()

-- | info logging to a file without an environment
fi :: (HasCallStack, MonadUnliftIO m) => RL () m a -> m a
fi = fie ()

-- | warn logging to a file without an environment
fw :: (HasCallStack, MonadUnliftIO m) => RL () m a -> m a
fw = fwe ()

-- | debug logging to a file with an environment
fde :: (HasCallStack, MonadUnliftIO m) => e -> RL e m a -> m a
fde e = logWith e logd

-- | info logging to a file with an environment
fie :: (HasCallStack, MonadUnliftIO m) => e -> RL e m a -> m a
fie e = logWith e (logx Info)

-- | warn logging to a file with an environment
fwe :: (HasCallStack, MonadUnliftIO m) => e -> RL e m a -> m a
fwe e = logWith e (logx Warn)

-- | create a long or short filename depending on the flag
setFileLong :: Bool -> LogOpts -> LogOpts
setFileLong b ll =
  ll{lLogFile = (\x -> x{fLongName = b}) <$> lLogFile ll}

-- | skip logging
lognone :: LogOpts
lognone =
  LogOpts
    Nothing
    Nothing
    Nothing
    False

-- | log to stdout in debug mode
logs :: LogOpts
logs =
  LogOpts
    Nothing
    (Just (Screen StdOut Debug))
    Nothing
    False

-- | log to the screen and a file using long file name in debug mode
logd :: LogOpts
logd = logx Debug

-- | log to the screen and a file using long file name
logx :: LLog -> LogOpts
logx lvl =
  LogOpts
    (Just (LogFile "def" True Debug "."))
    (Just (Screen StdOut lvl))
    Nothing
    False

-- | override the logging level
overrideLogLevel ::
  LLog ->
  Maybe LLog ->
  LogOpts ->
  LogOpts
overrideLogLevel deflvl mlvl opts = opts{lScreen = Just (Screen StdOut (fromMaybe deflvl mlvl))}

-- | newline for use in quasiquotes
newline :: Text
newline = "\n"

-- | output error message to stderr with a stack trace
werrS :: (HasCallStack, MonadIO m) => Text -> m ()
werrS = wmsgImpl True Error

-- | output error message to stderr
werr :: (HasCallStack, MonadIO m) => Text -> m ()
werr = wmsgImpl False Error

-- | output warning message to stderr with a stack trace
wwarnS :: (HasCallStack, MonadIO m) => Text -> m ()
wwarnS = wmsgImpl True Warn

-- | output warning message to stderr
wwarn :: (HasCallStack, MonadIO m) => Text -> m ()
wwarn = wmsgImpl False Warn

-- | output info message to stderr
winfo :: (HasCallStack, MonadIO m) => Text -> m ()
winfo = wmsgImpl False Info

-- | output debug message to stderr
wdebug :: (HasCallStack, MonadIO m) => Text -> m ()
wdebug = wmsgImpl False Debug

-- | output error message to stderr with a stacktrace and write to logger
werrLS :: (HasCallStack, MonadIO m, MonadLogger m) => Text -> m ()
werrLS = wmsgImplL True Error

-- | output error message to stderr and write to logger
werrL :: (HasCallStack, MonadIO m, MonadLogger m) => Text -> m ()
werrL = wmsgImplL False Error

-- | output warning message to stderr with a stacktrace and write to logger
wwarnLS :: (HasCallStack, MonadIO m, MonadLogger m) => Text -> m ()
wwarnLS = wmsgImplL True Warn

-- | output warning message to stderr and write to logger
wwarnL :: (HasCallStack, MonadIO m, MonadLogger m) => Text -> m ()
wwarnL = wmsgImplL False Warn

-- | output info message to stderr and write to logger
winfoL :: (HasCallStack, MonadIO m, MonadLogger m) => Text -> m ()
winfoL = wmsgImplL False Info

-- | output debug message to stderr and write to logger
wdebugL :: (HasCallStack, MonadIO m, MonadLogger m) => Text -> m ()
wdebugL = wmsgImplL False Debug

-- | output message to stderr with an optional stack trace
wmsgImpl ::
  (HasCallStack, MonadIO m) =>
  Bool ->
  LLog ->
  Text ->
  m ()
wmsgImpl stk lvl msg = liftIO $ do
  T.hPutStrLn stderr $ fst (dispLLog lvl) <> bool id getMessageCallStack stk msg
  hFlush stderr

-- | write to logger and stderr with optional stacktrace
wmsgImplL ::
  (HasCallStack, MonadIO m, MonadLogger m) =>
  Bool ->
  LLog ->
  Text ->
  m ()
wmsgImplL stk lvl msg = do
  let (x, y) = dispLLog lvl
  let msg' = bool id getMessageCallStack stk msg
  case lvl of
    Error -> $logError $ y <> msg'
    Warn -> $logWarn $ y <> msg'
    Info -> $logInfo $ y <> msg'
    Debug -> $logDebug $ y <> msg'
  liftIO $ do
    T.hPutStrLn stderr $ x <> msg'
    hFlush stderr

-- | pretty print stacktrace
getMessageCallStack ::
  HasCallStack =>
  Text ->
  Text
getMessageCallStack msg' =
  let cs = prettyCallStack callStack
   in msg' <> "\ncallstack=" <> T.pack cs

-- | pretty print 'LLog' settings for stderr output and file output
dispLLog :: LLog -> (Text, Text)
dispLLog =
  \case
    Debug -> ("[D] ", mempty)
    Info -> ("[I] ", mempty)
    Warn -> ("[W] ", "WARN: ")
    Error -> ("[E] ", "ERROR: ")

-- | holder for the start and end timers used to track job duration
data DiffTimer = DiffTimer
  { dfStartTimer :: !Timer
  , dfEndTimer :: !Timer
  , dfDurationPretty :: !Text
  , dfDurationSeconds :: !Int
  }
  deriving stock (Show)

-- | stop a timer after job has completed
stopTimer :: Timer -> IO DiffTimer
stopTimer start = diffTimers start =<< getTimer

-- | find the difference between a start and end timer
diffTimers :: Timer -> Timer -> IO DiffTimer
diffTimers dfStartTimer dfEndTimer = do
  let dfDurationSeconds = diffTimerSeconds dfStartTimer dfEndTimer
  let dfDurationPretty = secondsDuration dfDurationSeconds
  return DiffTimer{..}

-- | calculate the time in difference in seconds
diffTimerSeconds :: Timer -> Timer -> Int
diffTimerSeconds start end = timeSpecSeconds $ tmTimeSpec end - tmTimeSpec start

-- | create a new timer that starts now
getTimer :: IO Timer
getTimer = do
  tmZt <- getZonedTimeFloor
  tmTimeSpec <- getTime Monotonic
  let tmTimeString = T.pack (fmtZt tmZt)
  return Timer{..}

-- | holds timer information
data Timer = Timer
  { tmZt :: !ZonedTime
  , tmTimeSpec :: !TimeSpec
  , tmTimeString :: !Text
  }
  deriving stock (Show)

{-
-- used by proroar also uses fileNameDate (also roar843 diagnosis)
fmtShort, fmtLong, fmtLongCrazy :: FormatTime a => Format r (a -> r)
fmtShort = F.year <> F.month <> F.dayOfMonth
fmtLong = fmtShort <> "_" % F.hour24 <> F.minute <> F.second
fmtLongCrazy = fmtLong <> "." % F.pico
--- end used by
-}

{- | translates a haskell field to the dhall field without the prefix
   checks that the given a prefix matches for each of the fields
-}
fieldMod :: HasCallStack => Text -> D.InterpretOptions
fieldMod prefix =
  D.defaultInterpretOptions
    { D.fieldModifier = f
    }
 where
  f field =
    fromMaybe
      (normalError $ T.unpack $ "fieldMod:expected prefix[" <> prefix <> "] for field[" <> field <> "]")
      (T.stripPrefix prefix field)

instance FromDhall LLog
instance ToDhall LLog

instance FromDhall ScreenType
instance ToDhall ScreenType

instance FromDhall Email where
  autoWith _i = genericAutoWith (fieldMod "e")

instance ToDhall Email where
  injectWith _o = genericToDhallWith (fieldMod "e")

instance FromDhall LogFile where
  autoWith _i = genericAutoWith (fieldMod "f")

instance ToDhall LogFile where
  injectWith _o = genericToDhallWith (fieldMod "f")

instance FromDhall LogOpts where
  autoWith _i = genericAutoWith (fieldMod "l")

-- | decoder for 'LogOpts'
logopts :: Decoder LogOpts
logopts = genericAutoWith (fieldMod "l")

instance ToDhall LogOpts where
  injectWith _o = genericToDhallWith (fieldMod "l")

instance FromDhall Screen where
  autoWith _i = genericAutoWith (fieldMod "s")

instance ToDhall Screen where
  injectWith _o = genericToDhallWith (fieldMod "s")

-- | load a dhall expression
loadDhallTH :: forall a. (TH.Lift a, FromDhall a) => Text -> TH.Q TH.Exp
loadDhallTH txt = do
  c <- TH.runIO $ input (D.auto @a) txt
  TH.lift c

-- | load 'LogOpts' from a dhall expression
loadFromLogConfig :: Text -> IO LogOpts
loadFromLogConfig expr = do
  config <- input D.auto expr :: IO LogOpts
  T.putStrLn $ "configuration [" <> expr <> "] found:" <> T.pack (show config)
  return config

-- | load logger from a dhall expression and allow the caller to override the resulting 'LogOpts' and specify an environment
leWith :: MonadUnliftIO m => Text -> e -> (LogOpts -> LogOpts) -> RL e m a -> m a
leWith expr e g ma = do
  logcfg <- liftIO $ loadFromLogConfig expr
  logWith e (g logcfg) ma

-- | load logger from a default dhall file location and allow the caller to override the resulting 'LogOpts' and specify an environment
fbeWith :: MonadUnliftIO m => e -> (LogOpts -> LogOpts) -> RL e m a -> m a
fbeWith = leWith "./log.dhall" -- batch stuff

-- | load logger from a default dhall file location but skip the environment
fb :: MonadUnliftIO m => RL () m a -> m a
fb = fbeWith () id -- batch
