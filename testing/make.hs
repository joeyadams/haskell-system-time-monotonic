{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
import Development.Shake
import Development.Shake.FilePath
import System.Environment

tests =
    [ "benchmark"
    , "bombard"
    , "clock"
    , "delay"
    , "leak"
#if mingw32_HOST_OS
    , "diffMSec32"
#endif
    ]

#if mingw32_HOST_OS
binaries = map (++ ".exe") tests
#else
binaries = tests
#endif

intermediates = map (++ ".hi") tests
             ++ map (++ ".o")  tests

main = do
    args <- getArgs
    shake shakeOptions $ do
        case args of
            ["clean"] -> clean
            _         -> build

build = do
    want binaries
    (`elem` binaries) ?> \out -> do
        let hs = replaceExtension out ".hs"
        need [hs]
        system' "ghc" ["-Wall", "-O2", "-threaded", hs, "-o", out]

clean = action $ do
    system' "rm" $ "-f" : intermediates ++ binaries
