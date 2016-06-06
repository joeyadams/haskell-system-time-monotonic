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
-- every 24.8 days, if @GetTickCount@ is being used).
module System.Time.Monotonic.Direct (
    getSystemClock,
    SomeSystemClock(..),
    SystemClock(..),

    -- * Implementation(s)
    -- | The set of definitions below is platform-dependent.

#if mingw32_HOST_OS
    systemClock_GetTickCount,
    systemClock_GetTickCount64,
    systemClock_QueryPerformanceCounter,
#else
-- Yes, these #if defined()s work even though we #include <time.h> later; the compiler is smart
#if defined(CLOCK_BOOTTIME)
    systemClock_BOOTTIME,
#endif
#if defined(CLOCK_MONOTONIC_RAW)
    systemClock_MONOTONIC_RAW,
#endif
    systemClock_MONOTONIC,
    CTimeSpec,
#endif
) where

import Data.Bits        (isSigned)
import Data.Int
import Data.Time.Clock  (DiffTime)
import Data.Word
import Foreign          (Ptr, FunPtr, allocaBytes, nullFunPtr, peekByteOff)

#if mingw32_HOST_OS
import Data.Ratio ((%))
#include <Windows.h>
#else
import Foreign.C
#include <time.h>
#endif

-- | Existentially-quantified wrapper around 'SystemClock'
data SomeSystemClock = forall time cumtime.
                       SomeSystemClock (SystemClock time cumtime)

instance Show SomeSystemClock where
    showsPrec d (SomeSystemClock sc)
        = showParen (d > 10)
        $ showString "SomeSystemClock "
        . showsPrec 11 (systemClockName sc)

-- | A 'SystemClock' is a driver module used by 'System.Time.Monotonic.Clock'
-- to access a particular implementation of monotonic time support.
--
--  * @time@: Type of value returned by the system's time-getting function.
--
--  * @cumtime@: Type for accumulating differences between consecutive(-ish)
--    calls to 'systemClockGetTime', in case @time@ wraps around.
--    The reason we don't simply use 'DiffTime' is this: if the implementation
--    has to divide the result by a clock frequency, it could end up with a
--    number that is not an integral number of picoseconds.  Truncating to
--    'DiffTime' would lose precision, and that precision loss could add up, at
--    least in theory.
data SystemClock time cumtime = SystemClock
    { systemClockGetTime     :: IO time
    , systemClockDiffTime    :: time -> time -> cumtime
        -- ^ @systemClockDiffTime new old@ returns the amount of time that has
        -- elapsed between two calls to @systemClockGetTime@.
        --
        -- >systemClockDiffTime new old = new - old
        --
        -- This function should handle wraparound properly.  Also, bear in mind
        -- that @new@ may be earlier than @old@.  This can happen if multiple
        -- threads are accessing a 'System.Time.Monotonic.Clock'
        -- simultaneously.
        --
        -- Lastly, @systemClockDiffTime@ should not truncate precision in
        -- conversion to cumtime.  Otherwise, repeated calls to
        -- 'System.Time.Monotonic.clockGetTime' could degrade accuracy, due to
        -- lost precision adding up.
    , systemClockZeroCumTime :: cumtime
        -- ^ The number @0@.
    , systemClockAddCumTime  :: cumtime -> cumtime -> cumtime
        -- ^ Add two @cumtime@ values.  This should not overflow or lose
        -- precision.
    , systemClockCumToDiff   :: cumtime -> DiffTime
        -- ^ Convert a cumulative total of 'systemClockDiffTime' results to
        -- 'DiffTime'.  This may truncate precision if it needs to.
    , systemClockName        :: String
        -- ^ Label identifying this clock, like
        -- @\"clock_gettime(CLOCK_BOOTTIME)\"@ or
        -- @\"GetTickCount\"@.  This label is used for the 'Show'
        -- instances of 'SystemClock' and 'SomeSystemClock', and for
        -- 'System.Time.Monotonic.clockDriverName'.
    }

instance Show (SystemClock time cumtime) where
    showsPrec d sc
        = showParen (d > 10)
        $ showString "SystemClock "
        . showsPrec 11 (systemClockName sc)

-- | Return a module used for accessing the system's monotonic clock.  The
-- reason this is an 'IO' action, rather than simply a 'SystemClock' value, is
-- that the implementation may need to make a system call to determine what
-- monotonic time source to use, and how to use it.
getSystemClock :: IO SomeSystemClock
#if mingw32_HOST_OS
getSystemClock = do
    m <- systemClock_GetTickCount64
    case m of
        Just gtc64 -> return $ SomeSystemClock gtc64
        Nothing    -> return $ SomeSystemClock systemClock_GetTickCount
#elif defined(CLOCK_BOOTTIME)
getSystemClock =
    return $ SomeSystemClock systemClock_BOOTTIME
#else
getSystemClock =
    return $ SomeSystemClock systemClock_MONOTONIC
#endif

#if mingw32_HOST_OS

diffMSec32 :: Word32 -> Word32 -> DiffTime
diffMSec32 a b = fromIntegral (fromIntegral (a - b :: Word32) :: Int32) / 1000
    -- Do the subtraction modulo 2^32, to handle wraparound properly.
    -- However, convert it from unsigned to signed, to avoid
    -- a bogus result if a is earlier than b.

diffMSec64 :: Word64 -> Word64 -> DiffTime
diffMSec64 a b = fromIntegral (fromIntegral (a - b :: Word64) :: Int64) / 1000


-- | Use @GetTickCount@.  This is the default on Windows when @GetTickCount64@
-- is not available.
--
-- @GetTickCount@ has a 49.7 day wraparound, due to the type of the return
-- value (milliseconds as an unsigned 32-bit integer).
systemClock_GetTickCount :: SystemClock Word32 DiffTime
systemClock_GetTickCount =
    SystemClock
    { systemClockGetTime     = c_GetTickCount
    , systemClockDiffTime    = diffMSec32
    , systemClockZeroCumTime = 0
    , systemClockAddCumTime  = (+)
    , systemClockCumToDiff   = id
    , systemClockName        = "GetTickCount"
    }

foreign import stdcall "Windows.h GetTickCount"
    c_GetTickCount :: IO #{type DWORD}

-- | Uses @GetTickCount64@, which was introduced in Windows Vista and
-- Windows Server 2008.  This function tests, at runtime, if @GetTickCount64@
-- is available.
systemClock_GetTickCount64 :: IO (Maybe (SystemClock Word64 DiffTime))
systemClock_GetTickCount64 = do
    fun <- system_time_monotonic_load_GetTickCount64
    if fun == nullFunPtr
        then return Nothing
        else return $ Just $ clock $ mkGetTickCount64 fun
  where
    clock getTickCount64 =
        SystemClock
        { systemClockGetTime     = getTickCount64
        , systemClockDiffTime    = diffMSec64
        , systemClockZeroCumTime = 0
        , systemClockAddCumTime  = (+)
        , systemClockCumToDiff   = id
        , systemClockName        = "GetTickCount64"
        }

type C_GetTickCount64 = IO #{type ULONGLONG}

-- Defined in cbits/dll.c
foreign import ccall
    system_time_monotonic_load_GetTickCount64 :: IO (FunPtr C_GetTickCount64)

foreign import stdcall "dynamic"
    mkGetTickCount64 :: FunPtr C_GetTickCount64 -> C_GetTickCount64

qpcDiffTime :: Int64 -> Int64 -> Integer
qpcDiffTime new old = fromIntegral (new - old)

qpcCumToDiff :: Int64 -> Integer -> DiffTime
qpcCumToDiff freq cum = fromRational (cum % fromIntegral freq)

-- | Uses @QueryPerformanceCounter@.  This is not the default because it is
-- less reliable in the long run than @GetTickCount@.  On my laptop, it skips
-- ahead roughly 2 seconds when the computer goes into sleep mode, but counts
-- time spent sleeping.
systemClock_QueryPerformanceCounter :: IO (Maybe (SystemClock Int64 Integer))
systemClock_QueryPerformanceCounter = do
    mfreq <- callQP c_QueryPerformanceFrequency
    case mfreq of
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
            , systemClockDiffTime    = qpcDiffTime
            , systemClockZeroCumTime = 0
            , systemClockAddCumTime  = (+)
            , systemClockCumToDiff   = qpcCumToDiff freq
            , systemClockName        = "QueryPerformanceCounter"
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

#else

type Time_t = #{type time_t}

data CTimeSpec = CTimeSpec
    { tv_sec    :: !Time_t
        -- ^ seconds
    , tv_nsec   :: !CLong
        -- ^ nanoseconds.  1 second = 10^9 nanoseconds
    }
    deriving Show

diffCTimeSpec :: CTimeSpec -> CTimeSpec -> DiffTime
diffCTimeSpec a b
  = diffCTime (tv_sec a) (tv_sec b)
  + fromIntegral (tv_nsec a - tv_nsec b) / 1000000000

diffCTime :: Time_t -> Time_t -> DiffTime
diffCTime a b
    | isSigned a = fromIntegral (a - b)
    | otherwise  = error "System.Time.Monotonic.Direct: time_t is unsigned"
        -- time_t is supposed to be signed on POSIX systems.
        -- If a is earlier than b, unsigned subtraction will produce an
        -- enormous result.

peekCTimeSpec :: Ptr CTimeSpec -> IO CTimeSpec
peekCTimeSpec ptr = do
    sec  <- #{peek struct timespec, tv_sec}  ptr
    nsec <- #{peek struct timespec, tv_nsec} ptr
    return CTimeSpec { tv_sec  = sec
                     , tv_nsec = nsec
                     }

-- | Uses @clock_gettime@ with @CLOCK_MONOTONIC@.
--
-- /Warning:/ on Linux, this clock stops when the computer is suspended.
-- See <http://lwn.net/Articles/434239/>.
systemClock_MONOTONIC :: SystemClock CTimeSpec DiffTime
systemClock_MONOTONIC =
    SystemClock
    { systemClockGetTime     = clock_gettime #{const CLOCK_MONOTONIC}
    , systemClockDiffTime    = diffCTimeSpec
    , systemClockZeroCumTime = 0
    , systemClockAddCumTime  = (+)
    , systemClockCumToDiff   = id
    , systemClockName        = "clock_gettime(CLOCK_MONOTONIC)"
    }

#if defined(CLOCK_BOOTTIME)
-- | Uses @clock_gettime@ with @CLOCK_BOOTTIME@.
systemClock_BOOTTIME :: SystemClock CTimeSpec DiffTime
systemClock_BOOTTIME =
    SystemClock
    { systemClockGetTime     = clock_gettime #{const CLOCK_BOOTTIME}
    , systemClockDiffTime    = diffCTimeSpec
    , systemClockZeroCumTime = 0
    , systemClockAddCumTime  = (+)
    , systemClockCumToDiff   = id
    , systemClockName        = "clock_gettime(CLOCK_BOOTTIME)"
    }
#endif

#if defined(CLOCK_MONOTONIC_RAW)
-- | Uses @clock_gettime@ with @CLOCK_MONOTONIC_RAW@.
--
-- CLOCK_MONOTONIC_RAW is more reliable; according to @man 2 clock_gettime@, it
-- is not subject to "NTP adjustments or [..] adjtime(3)". However, like
-- @CLOCK_MONOTONIC@ it stops when the computer is suspended.
systemClock_MONOTONIC_RAW :: SystemClock CTimeSpec DiffTime
systemClock_MONOTONIC_RAW =
    SystemClock
    { systemClockGetTime     = clock_gettime #{const CLOCK_MONOTONIC_RAW}
    , systemClockDiffTime    = diffCTimeSpec
    , systemClockZeroCumTime = 0
    , systemClockAddCumTime  = (+)
    , systemClockCumToDiff   = id
    , systemClockName        = "clock_gettime(CLOCK_MONOTONIC_RAW)"
    }
#endif

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
