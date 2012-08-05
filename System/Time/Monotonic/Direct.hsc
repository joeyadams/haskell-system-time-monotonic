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
-- but provides less protection against wraparound.
--
-- More specifically, in the higher-level "System.Time.Monotonic" API,
-- 'System.Time.Monotonic.Clock' updates its internal disposition every time
-- 'System.Time.Monotonic.clockGetTime' is called.  The only way to get a
-- wraparound issue with the higher-level API is to call
-- 'System.Time.Monotonic.clockGetTime' very seldomly (e.g. less than once
-- every 49.7 days, if @GetTickCount@ is being used).
module System.Time.Monotonic.Direct (
    getSystemClock,
    SomeSystemClock(..),
    SystemClock(..),

    -- * Implementation(s)
    -- | The set of definitions below is platform-dependent.

#if mingw32_HOST_OS
    systemClock_QueryPerformanceCounter,
    systemClock_GetTickCount,
#else
    systemClock_MONOTONIC,
    CTimeSpec,
#endif
) where

import Data.Int
import Data.Time.Clock  (DiffTime)
import Data.Word

#if mingw32_HOST_OS
import Data.Ratio ((%))
#include <Windows.h>
#else
import Foreign
import Foreign.C
#include <time.h>
#endif

-- | Existentially-quantified wrapper around 'SystemClock'
data SomeSystemClock = forall time. SomeSystemClock (SystemClock time)

data SystemClock time = SystemClock
    { systemClockGetTime  :: IO time
    , systemClockDiffTime :: time -> time -> DiffTime
        -- ^ @systemClockDiffTime new old@ returns the amount of time that has
        -- elapsed between two calls to @systemClockGetTime@.
        --
        -- This function should obey the following law:
        --
        -- >systemClockDiffTime new old == -(systemClockDiffTime old new)
        --
        -- That is, if @new < old@, @systemClockDiffTime@ should not return
        -- something weird because it thinks overflow occurred.  Two threads
        -- using a single 'System.Time.Monotonic.Clock' might ask for the time
        -- simultaneously, possibly resulting in @new@ being before @old@.
    , systemClockName     :: String
        -- ^ Label identifying this clock, like
        -- @\"clock_gettime(CLOCK_MONOTONIC)\"@ or
        -- @\"QueryPerformanceCounter\"@.
    }

-- | Return a module used for accessing the system's monotonic clock.  The
-- reason this is an 'IO' action, rather than simply a 'SystemClock' value, is
-- that the implementation may need to make a system call to determine what
-- monotonic time source to use, and how to use it.
getSystemClock :: IO SomeSystemClock
#if mingw32_HOST_OS
getSystemClock = do
    m <- systemClock_QueryPerformanceCounter
    case m of
        Just qpc -> return $ SomeSystemClock qpc
        Nothing  -> return $ SomeSystemClock systemClock_GetTickCount
#else
getSystemClock =
    return $ SomeSystemClock systemClock_MONOTONIC
#endif

#if mingw32_HOST_OS

qpcDiffTime :: Int64 -> Int64 -> Int64 -> DiffTime
qpcDiffTime freq new old =
    fromRational $ fromIntegral (new - old) % freq

systemClock_QueryPerformanceCounter :: IO (Maybe (SystemClock Int64))
systemClock_QueryPerformanceCounter = do
    m <- callQP c_QueryPerformanceFrequency
    case m of
        Nothing   -> return Nothing
        Just 0    -> return Nothing -- Shouldn't happen; just a safeguard to
                                    -- prevent zero denominator in 'qpcDiffTime'.
        Just freq -> return $ Just SystemClock
            { systemClockGetTime = do
                m <- callQP c_QueryPerformanceCounter
                case m of
                    Just t  -> return t
                    Nothing -> fail "QueryPerformanceCounter failed,\
                                    \ even though QueryPerformanceFrequency\
                                    \ succeeded earlier"
            , systemClockDiffTime = qpcDiffTime freq
            , systemClockName     = "QueryPerformanceCounter"
            }

systemClock_GetTickCount :: SystemClock Word32
systemClock_GetTickCount =
    SystemClock
    { systemClockGetTime  = c_GetTickCount
    , systemClockDiffTime = \a b -> fromIntegral (a - b :: Word32) / 1000
                                    -- NB: Do the subtraction modulo 2^32,
                                    --     to handle wraparound properly.
    , systemClockName     = "GetTickCount"
    }

callQP :: QPFunc -> IO (Maybe Int64)
callQP qpfunc =
    allocaBytes #{size LARGE_INTEGER} $ \ptr -> do
        ok <- qpfunc ptr
        if ok /= 0
            then do
                n <- #{peek LARGE_INTEGER, QuadPart} ptr
                return (Just n)
            else return Nothing

type QPFunc = Ptr Int64 -> IO #{type BOOL}

foreign import stdcall "Windows.h QueryPerformanceFrequency"
    c_QueryPerformanceFrequency :: QPFunc

foreign import stdcall "Windows.h QueryPerformanceCounter"
    c_QueryPerformanceCounter :: QPFunc

foreign import stdcall "Windows.h GetTickCount"
    c_GetTickCount :: IO #{type DWORD}

#else

data CTimeSpec = CTimeSpec
    { tv_sec    :: !(#{type time_t})
        -- ^ seconds
    , tv_nsec   :: !CLong
        -- ^ nanoseconds.  1 second = 10^9 nanoseconds
    }

diffCTimeSpec :: CTimeSpec -> CTimeSpec -> DiffTime
diffCTimeSpec a b
  = fromIntegral (tv_sec a - tv_sec b)
  + fromIntegral (tv_nsec a - tv_nsec b) / 1000000000

peekCTimeSpec :: Ptr CTimeSpec -> IO CTimeSpec
peekCTimeSpec ptr = do
    sec  <- #{peek struct timespec, tv_sec}  ptr
    nsec <- #{peek struct timespec, tv_nsec} ptr
    return CTimeSpec { tv_sec  = sec
                     , tv_nsec = nsec
                     }

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
