-- |
-- Module:      System.Time.Monotonic
-- Copyright:   (c) Joseph Adams 2012
-- License:     BSD3
-- Maintainer:  joeyadams3.14159@gmail.com
-- Portability: Tested on Linux and Windows.
--
-- This module provides a platform-independent API for using the system's
-- monotonic clock.
--
-- Known issues:
--
--  * On Windows XP, this uses @GetTickCount@, which has a 49.7 day wraparound.
--    'Clock' works around this problem, but the workaround only works if
--    'clockGetTime' is called at least once every 24.8 days.
--
--  * On Linux, this uses @clock_gettime@ with @CLOCK_MONOTONIC@,
--    which (unfortunately) stops when the computer is suspended.  Thus,
--    'clockGetTime' will not include time spent sleeping.  Do not rely on this
--    behavior, as it may be fixed in a future version of this library.
{-# LANGUAGE ExistentialQuantification #-}
module System.Time.Monotonic (
    -- * Clock
    Clock,
    newClock,
    clockGetTime,

    -- ** Drivers
    newClockWithDriver,
    clockDriverName,

    -- * Utilities
    delay,
) where

import System.Time.Monotonic.Direct

import Control.Concurrent   (threadDelay)
import Data.IORef
import Data.Time.Clock      (DiffTime)

data Clock = forall time. Clock !(SystemClock time) !(IORef (ClockData time))

-- We can't have the Eq instance because the time type is existentially
-- quantified, meaning the equality below would have to compare two IORefs of
-- different value type.  If we really wanted it, we could add a dummy IORef,
-- use Typeable, or perhaps even use unsafeCoerce.
--
-- instance Eq Clock where
--     Clock _ a == Clock _ b = a == b

-- | The externally reported time, paired with the return value of
-- 'systemClockGetTime' at a given point in time.
--
-- The disposition between the 'DiffTime' and the @time@ is set when 'newClock'
-- is called, and remains constant for the lifetime of the 'Clock'.
-- 'clockGetTime' merely increments both quantities by the same amount.
-- Therefore, barring precision loss, calling 'clockGetTime' frequently should
-- not degrade the accuracy of the clock.
data ClockData time = ClockData !DiffTime !time

-- | Create a new 'Clock'.  The result of 'clockGetTime' is based on the time
-- 'newClock' was called.
newClock :: IO Clock
newClock = newClockWithDriver =<< getSystemClock

-- | Return the amount of time that has elapsed since the clock was created
-- with 'newClock'.
clockGetTime :: Clock -> IO DiffTime
clockGetTime (Clock clock ref) = do
    st2 <- systemClockGetTime clock
    t2 <- atomicModifyIORef ref $
        \(ClockData t1 st1) ->
            let t2 = t1 + systemClockDiffTime clock st2 st1
             in (ClockData t2 st2, t2)
    t2 `seq` return t2

-- | Variant of 'newClock' that uses the given driver.  This can be used if you
-- want to use a different time source than the default.
--
-- @'newClock' = 'newClockWithDriver' =<< 'getSystemClock'@
newClockWithDriver :: SomeSystemClock -> IO Clock
newClockWithDriver (SomeSystemClock clock) = do
    st <- systemClockGetTime clock
    ref <- newIORef (ClockData 0 st)
    return (Clock clock ref)

-- | Return a string identifying the time source, such as
-- @\"clock_gettime(CLOCK_MONOTONIC)\"@ or
-- @\"GetTickCount\"@.
clockDriverName :: Clock -> String
clockDriverName (Clock clock _) = systemClockName clock

-- | Variant of 'threadDelay' for 'DiffTime'.
delay :: DiffTime -> IO ()
delay difftime =
    loop $ ceiling $ difftime * 1000000
  where
    loop :: Integer -> IO ()
    loop usec
        | usec <= maxWait = wait usec
        | otherwise       = wait maxWait
                         >> loop (usec - maxWait)

    -- maxWait is 100 seconds.  2^31-1 microseconds is about 2147 seconds.
    -- This gives the implementation of 'threadDelay' plenty of leeway if it
    -- needs to do some arithmetic on the time value first.
    maxWait = 100000000

    wait usec = case fromIntegral usec of
        n | n > 0     -> threadDelay n
          | otherwise -> return ()
            -- Guard against negative argument to threadDelay,
            -- for earlier versions of base.
            -- See http://hackage.haskell.org/trac/ghc/ticket/2892
