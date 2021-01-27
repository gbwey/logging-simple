{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module DateTA where
import           Control.Monad.ST        (ST)
import qualified Data.Text.Array         as TA
import           Data.Text               (Text)
import           Data.Text.Internal      (Text(..))
import           Data.Word               (Word16)
#ifdef mingw32_HOST_OS
import System.Win32.Time (SYSTEMTIME(..))
#endif
import Data.Fixed (div', divMod')
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)

ioDatePosixKatip :: TimeZone -> IO Text
ioDatePosixKatip tz = do
  tm <- getPOSIXTime
  let y = utcToZonedTime tz (posixSecondsToUTCTime tm)
  return $ formatAsTimeStampP y
{-# INLINEABLE ioDatePosixKatip #-}

#ifdef mingw32_HOST_OS
formatAsFileNameLong :: SYSTEMTIME -> Text
formatAsFileNameLong SYSTEMTIME{..} = toText $ TA.run2 $ do
   buf <- TA.new 18 -- length "20161020_123456
   _ <- writeDayFile buf 0 wYear wMonth wDay
   TA.unsafeWrite buf 8 0x5F -- underscore
   next <- writeTimeOfDayFile buf 9 wHour wMinute wSecond
   return (buf, next)
  where
     toText (arr, len) = Text arr 0 len
{-# INLINEABLE formatAsFileNameLong #-}

formatAsFileNameShort :: SYSTEMTIME -> Text
formatAsFileNameShort SYSTEMTIME{..} = toText $ TA.run2 $ do
   buf <- TA.new 8 -- length "20161020_123456
   next <- writeDayFile buf 0 wYear wMonth wDay
   return (buf, next)
  where
     toText (arr, len) = Text arr 0 len
{-# INLINEABLE formatAsFileNameShort #-}


formatAsTimeStampST :: SYSTEMTIME -> Text
formatAsTimeStampST SYSTEMTIME{..} = toText $ TA.run2 $ do
   buf <- TA.new 23 -- length "2016-10-20T12:34:56.123"
   _ <- writeDay buf 0 wYear wMonth wDay
   TA.unsafeWrite buf  10  0x54 -- T
   next <- writeTimeOfDay buf 11 wHour wMinute wSecond wMilliseconds
   return (buf, next)
  where
     toText (arr, len) = Text arr 0 len
{-# INLINEABLE formatAsTimeStampST #-}
#endif

formatAsTimeStampP :: ZonedTime -> Text
formatAsTimeStampP zt = toText $ TA.run2 $ do
   let LocalTime dy (TimeOfDay hh mm pp) = zonedTimeToLocalTime zt
   buf <- TA.new 23 -- length "2016-10-20T12:34:56.123"
   let (wYear, wMonth, wDay) = toGregorian dy
   _ <- writeDay buf 0 (fromIntegral wYear) (fromIntegral wMonth) (fromIntegral wDay)
   TA.unsafeWrite buf 10 0x54 -- T
   let (ss,s12) = divMod' pp 1
   let ms = div' s12 0.001
   next <- writeTimeOfDay buf 11 (fromIntegral hh) (fromIntegral mm) ss ms
   return (buf, next)
  where
     toText (arr, len) = Text arr 0 len
{-# INLINEABLE formatAsTimeStampP #-}

-- | Writes the @YYYY-MM-DD@ part of timestamp
writeDay :: TA.MArray s -> Int -> Word16 -> Word16 -> Word16 -> ST s Int
writeDay buf off yr m d =
  do
    TA.unsafeWrite buf (off + 0) (digit y1)
    TA.unsafeWrite buf (off + 1) (digit y2)
    TA.unsafeWrite buf (off + 2) (digit y3)
    TA.unsafeWrite buf (off + 3) (digit y4)
    TA.unsafeWrite buf (off + 4) 0x2d -- dash
    TA.unsafeWrite buf (off + 5) m1
    TA.unsafeWrite buf (off + 6) m2
    TA.unsafeWrite buf (off + 7) 0x2d -- dash
    TA.unsafeWrite buf (off + 8) d1
    TA.unsafeWrite buf (off + 9) d2
    return (off + 10)
  where
    (y1, ya) = abs yr `quotRem` 1000
    (y2, yb) = ya `quotRem` 100
    (y3, y4) = yb `quotRem` 10
    T m1 m2  = twoDigits m
    T d1 d2  = twoDigits d
{-# INLINE writeDay #-}

-- | Write time of day, optionally with sub seconds
writeTimeOfDay :: TA.MArray s -> Int -> Word16 -> Word16 -> Word16 -> Word16 -> ST s Int
writeTimeOfDay buf off hh mm ss mi =
  do
    TA.unsafeWrite buf  off      h1
    TA.unsafeWrite buf (off + 1) h2
    TA.unsafeWrite buf (off + 2) 0x3A -- colon
    TA.unsafeWrite buf (off + 3) m1
    TA.unsafeWrite buf (off + 4) m2
    TA.unsafeWrite buf (off + 5) 0x3A -- colon
    TA.unsafeWrite buf (off + 6) s1
    TA.unsafeWrite buf (off + 7) s2
    TA.unsafeWrite buf (off + 8) 0x2E -- dot
    TA.unsafeWrite buf (off + 9) (digit mi1)
    TA.unsafeWrite buf (off + 10) (digit mi3)
    TA.unsafeWrite buf (off + 11) (digit mi4)
    return (off + 12)
  where
   T h1 h2 = twoDigits hh
   T m1 m2 = twoDigits mm
   T s1 s2 = twoDigits ss
   (mi1, mi2) = mi `quotRem` 100
   (mi3, mi4) = mi2 `quotRem` 10
{-# INLINE writeTimeOfDay #-}

-- Following code was adapted from aeson package.
--
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3

data T = T {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16

twoDigits :: Word16 -> T
twoDigits a     = T (digit hi) (digit lo)
  where (hi,lo) = a `quotRem` 10

digit :: Word16 -> Word16
digit x = x + 48

-- | Writes the @YYYY-MM-DD@ part of timestamp
writeDayFile :: TA.MArray s -> Int -> Word16 -> Word16 -> Word16 -> ST s Int
writeDayFile buf off yr m d =
  do
    TA.unsafeWrite buf (off + 0) (digit y1)
    TA.unsafeWrite buf (off + 1) (digit y2)
    TA.unsafeWrite buf (off + 2) (digit y3)
    TA.unsafeWrite buf (off + 3) (digit y4)
    TA.unsafeWrite buf (off + 4) m1
    TA.unsafeWrite buf (off + 5) m2
    TA.unsafeWrite buf (off + 6) d1
    TA.unsafeWrite buf (off + 7) d2
    return (off + 8)
  where
    (y1, ya) = abs yr `quotRem` 1000
    (y2, yb) = ya `quotRem` 100
    (y3, y4) = yb `quotRem` 10
    T m1 m2  = twoDigits m
    T d1 d2  = twoDigits d
{-# INLINE writeDayFile #-}

-- | Write time of day, optionally with sub seconds
writeTimeOfDayFile :: TA.MArray s -> Int -> Word16 -> Word16 -> Word16 -> ST s Int
writeTimeOfDayFile buf off hh mm ss =
  do
    TA.unsafeWrite buf  off      h1
    TA.unsafeWrite buf (off + 1) h2
    TA.unsafeWrite buf (off + 2) m1
    TA.unsafeWrite buf (off + 3) m2
    TA.unsafeWrite buf (off + 4) s1
    TA.unsafeWrite buf (off + 5) s2
    return (off + 6)
  where
   T h1 h2 = twoDigits hh
   T m1 m2 = twoDigits mm
   T s1 s2 = twoDigits ss
{-# INLINE writeTimeOfDayFile #-}

