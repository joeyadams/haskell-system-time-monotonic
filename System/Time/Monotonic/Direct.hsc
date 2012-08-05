{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module:      System.Time.Monotonic.Direct
-- Copyright:   (c) Joseph Adams 2012
-- License:     BSD3
-- Maintainer:  joeyadams3.14159@gmail.com
-- Portability: Tested on Linux and Windows
--
-- This module provides more direct access to the system's monotonic clock,
-- provides less protection against wraparound.  The higher-level
-- 'System.Time.Monotonic.Clock' updates its internal disposition every time
-- 'System.Time.Monotonic.clockGetTime' is called, so the only way to get
-- a wraparound issue is to call 'System.Time.Monotonic.clockGetTime'
-- very seldomly (e.g. less than once every 49.7 days, if @GetTickCount@ is
-- being used).
module System.Time.Monotonic.Direct (
    getSystemClock,
    SomeSystemClock(..),
    SystemClock(..),
    MSec(..),

    -- * Implementation(s)
    -- | The set of definitions below is platform-dependent.

#if mingw32_HOST_OS
    systemClock_GetTickCount,
#else
    CTimeSpec(..),
    systemClock_MONOTONIC,
#endif
) where

import Data.Int
import Data.Word

#if mingw32_HOST_OS
#include <Windows.h>
#else
import Foreign
import Foreign.C
#include <time.h>
#endif

-- | A duration, measured in milliseconds.
newtype MSec = MSec Int64
    deriving (Eq, Ord, Show)

-- | Existentially-quantified wrapper around 'SystemClock'
data SomeSystemClock = forall time. SomeSystemClock (SystemClock time)

data SystemClock time = SystemClock
    { systemClockGetTime  :: IO time
    , systemClockDiffTime :: time -> time -> MSec
        -- ^ @systemClockDiffTime new old@ returns the amount of time that has
        -- elapsed between two calls to @systemClockGetTime@.
        --
        -- This function should obey the following law:
        --
        -- >systemClockDiffTime a b = -(systemClockDiffTime b a)
        --
        -- That is, if @new < old@, 'systemClockDiffTime' should not return
        -- something weird because it thinks overflow occurred.  Two threads
        -- using a single 'System.Time.Monotonic.Clock' might ask for the time
        -- simultaneously, possibly resulting in @new@ being before @old@.
    , systemClockName     :: String
        -- ^ Label identifying this clock, like
        -- @\"clock_gettime(CLOCK_MONOTONIC)\"@ or @\"GetTickCount\"@.
    }

-- | Return a module used for accessing the system's monotonic clock.  The
-- reason this is an 'IO' action, rather than simply a 'SystemClock' value, is
-- that the implementation may need to make a system call to determine what
-- monotonic time source to use.
getSystemClock :: IO SomeSystemClock
#if mingw32_HOST_OS
getSystemClock =
    return $ SomeSystemClock systemClock_GetTickCount
#else
getSystemClock =
    return $ SomeSystemClock systemClock_MONOTONIC
#endif

#if mingw32_HOST_OS

systemClock_GetTickCount :: SystemClock Word32
systemClock_GetTickCount =
    SystemClock
    { systemClockGetTime  = c_GetTickCount
    , systemClockDiffTime = \a b -> fromIntegral (a - b)
    , systemClockName     = "GetTickCount"
    }

foreign import stdcall "Windows.h GetTickCount"
    c_GetTickCount :: IO #{type DWORD}

#else

data CTimeSpec = CTimeSpec
    { tv_sec    :: !(#{type time_t})
        -- ^ seconds
    , tv_nsec   :: !CLong
        -- ^ nanoseconds.  1 second = 10^9 nanoseconds
    }

peekCTimeSpec :: Ptr CTimeSpec -> IO CTimeSpec
peekCTimeSpec ptr = do
    sec  <- #{peek struct timespec, tv_sec}  ptr
    nsec <- #{peek struct timespec, tv_nsec} ptr
    return CTimeSpec { tv_sec  = sec
                     , tv_nsec = nsec
                     }

diffCTimeSpec :: CTimeSpec -> CTimeSpec -> MSec
diffCTimeSpec = undefined -- TODO

-- | @clock_gettime(CLOCK_MONOTONIC)@
systemClock_MONOTONIC :: SystemClock CTimeSpec
systemClock_MONOTONIC =
    SystemClock
    { systemClockGetTime  = clock_gettime #{const CLOCK_MONOTONIC}
    , systemClockDiffTime = diffCTimeSpec
    , systemClockName     = "clock_gettime(CLOCK_MONOTONIC)"
    }

-- CLOCK_MONOTONIC_RAW is more reliable, but requires
-- a recent kernel and glibc.
--
-- -- | @clock_gettime(CLOCK_MONOTONIC_RAW)@
-- systemClock_MONOTONIC_RAW :: SystemClock CTimeSpec
-- systemClock_MONOTONIC_RAW =
--     SystemClock
--     { systemClockGetTime  = clock_gettime #{const CLOCK_MONOTONIC_RAW}
--     , systemClockDiffTime = diffCTimeSpec -- TODO
--     , systemClockName     = "clock_gettime(CLOCK_MONOTONIC_RAW)"
--     }

clock_gettime :: #{type clockid_t} -> IO CTimeSpec
clock_gettime clk_id =
    allocaBytes #{size struct timespec} $ \ptr -> do
        throwErrnoIfMinus1_ "clock_gettime" $
            c_clock_gettime clk_id ptr
        peekCTimeSpec ptr

foreign import ccall "time.h clock_gettime"
    c_clock_gettime :: #{type clockid_t}
                    -> Ptr CTimeSpec
                    -> IO CInt

#endif
