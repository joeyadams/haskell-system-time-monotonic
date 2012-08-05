import System.Time.Monotonic.Direct
import Data.Time.Clock (DiffTime)

test :: DiffTime -> CTimeSpec -> CTimeSpec -> IO Bool
test expected new old =
    return $ diffCTimeSpec new old == expected
          && diffCTimeSpec old new == -expected
  where
    diffCTimeSpec = systemClockDiffTime systemClock_MONOTONIC

main :: IO ()
main = do
    True <- test 0.000000001
        (CTimeSpec 100 999999999)
        (CTimeSpec 100 999999998)

    True <- test 0.000000002
        (CTimeSpec 101         1)
        (CTimeSpec 100 999999999)

    True <- test 0.000000002
        (CTimeSpec minBound         1)
        (CTimeSpec maxBound 999999999)

    True <- test 1.999999998
        (CTimeSpec minBound 999999999)
        (CTimeSpec maxBound         1)

    True <- test 1.0
        (CTimeSpec minBound 0)
        (CTimeSpec maxBound 0)

    True <- test 0.0
        (CTimeSpec 0 0)
        (CTimeSpec 0 0)

    putStrLn "All tests passed"
