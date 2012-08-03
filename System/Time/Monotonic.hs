-- |
-- Module:      System.Time.Monotonic
-- Copyright:   (c) Joseph Adams 2012
-- License:     BSD3
-- Maintainer:  joeyadams3.14159@gmail.com
-- Portability: Tested on Linux and Windows.
--
-- Simple library for using the system's monotonic clock.  This library is
-- intended to be portable and reliable, but does not (necessarily) provide
-- high-resolution timing.
--
-- On Windows, this uses @GetTickCount@, and on Linux, it uses @clock_gettime@
-- with @CLOCK_MONOTONIC@.  In both cases, the 'Clock' object avoids wraparound
-- issues by getting the system time periodically, to avoid comparing two
-- system times that are too far apart.
module System.Time.Monotonic (
    -- * Clock
    Clock,
    newClock,
    clockGetTime,

    -- * Time representation
    MSec(..),
) where

import System.Time.Monotonic.Direct (getMonotonicTimeMSec32)

import Control.Concurrent
import Data.Int             (Int64)
import Data.IORef
import Data.Word            (Word32)
import System.Mem.Weak      (deRefWeak)

-- | A duration, measured in milliseconds.
newtype MSec = MSec Int64
    deriving (Eq, Ord, Show)

newtype Clock = Clock (IORef ClockData)
    deriving Eq

-- | The externally reported time, paired with the return value of
-- 'getMonotonicTimeMSec32', at a given point in time.
data ClockData = ClockData !Int64 !Word32

-- | Create a new 'Clock'.
--
-- Performance consideration: this will fork a thread that periodically gets
-- the time and updates the clock's internal disposition.  That thread will go
-- away when the 'Clock' is garbage collected.
newClock :: IO Clock
newClock = do
    st <- getMonotonicTimeMSec32
    ref <- newIORef (ClockData 0 st)
    nudgePeriodically (Clock ref)
    return (Clock ref)

-- | Return the amount of time that has elapsed since the clock was created
-- with 'newClock'.
clockGetTime :: Clock -> IO MSec
clockGetTime (Clock ref) = do
    st2 <- getMonotonicTimeMSec32
    t2 <- atomicModifyIORef ref $
        \(ClockData t1 st1) ->
            let t2 = t1 + fromIntegral (st2 - st1)
             in (ClockData t2 st2, t2)
    t2 `seq` return (MSec t2)

nudgePeriodically :: Clock -> IO ()
nudgePeriodically (Clock ref) = do
    weak_mv <- newEmptyMVar

    tid <- forkIO $ do
        weak <- takeMVar weak_mv
        let loop = do
                m <- deRefWeak weak
                case m of
                    Nothing   -> return ()
                    Just ref' -> do
                        _ <- clockGetTime (Clock ref')
                        threadDelay 60000000
                        loop
         in loop

    mkWeakIORef ref (killThread tid)
        >>= putMVar weak_mv
