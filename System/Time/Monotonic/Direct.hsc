-- |
-- Module:      System.Time.Monotonic.Direct
-- Copyright:   (c) Joseph Adams 2012
-- License:     BSD3
-- Maintainer:  joeyadams3.14159@gmail.com
-- Portability: Tested on Linux and Windows
--
-- This module provides more direct access to the system's monotonic clock, but
-- does not take care of the 49.7 day wraparound issue for you.  Use with care.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module System.Time.Monotonic.Direct (
    getMonotonicTimeMSec32,
) where

import Data.Word

#if mingw32_HOST_OS
#include <Windows.h>
#else
import Foreign
import Foreign.C
#include <time.h>
#endif

-- | Get the time from the system's monotonic clock, in milliseconds.  Due to
-- the use of 'Word32', this has a wraparound of about 49.7 days.  However,
-- this is not a problem, provided that you:
--
--  * Don't compare two results of 'getMonotonicTimeMSec32' that are more than 49.7
--    days apart.
--
--  * Perform the subtraction on 'Word32' times.
--
-- Do this:
--
-- >now <- getMonotonicTimeMSec32
-- >let elapsed = fromIntegral (now - before)
--
-- Not this:
--
-- >now <- getMonotonicTimeMSec32
-- >let elapsed = fromIntegral now - fromIntegral before -- WRONG
--
-- When overflow occurs, subtraction modulo 2^32 will do the right thing.
-- See <http://stackoverflow.com/a/9232314/149391>.
--
-- On Windows, this is just @GetTickCount@.  For systems that provide
-- additional precision or have a larger wraparound, that information is simply
-- truncated out.  This keeps overflow handling simple and consistent.
getMonotonicTimeMSec32 :: IO Word32

#if mingw32_HOST_OS

getMonotonicTimeMSec32 = c_GetTickCount

foreign import stdcall "Windows.h GetTickCount"
    c_GetTickCount :: IO #{type DWORD}

#else

getMonotonicTimeMSec32 =
    allocaBytes #{size struct timespec} $ \ptr -> do
        throwErrnoIfMinus1_ "clock_gettime" $
            c_clock_gettime #{const CLOCK_MONOTONIC} ptr
        t <- peekCTimeSpec ptr
        return $! fromIntegral (tv_sec t) * 1000 + fromIntegral (tv_nsec t `div` 1000000)

foreign import ccall "time.h clock_gettime"
    c_clock_gettime :: #{type clockid_t}
                    -> Ptr CTimeSpec
                    -> IO CInt

data CTimeSpec = CTimeSpec
    { tv_sec    :: !(#{type time_t})
    , tv_nsec   :: !CLong
    }

peekCTimeSpec :: Ptr CTimeSpec -> IO CTimeSpec
peekCTimeSpec ptr = do
    sec  <- #{peek struct timespec, tv_sec}  ptr
    nsec <- #{peek struct timespec, tv_nsec} ptr
    return CTimeSpec { tv_sec  = sec
                     , tv_nsec = nsec
                     }

#endif
