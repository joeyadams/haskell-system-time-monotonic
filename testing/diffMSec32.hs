import System.Time.Monotonic.Direct
import Data.Word        (Word32)
import Data.Time.Clock  (DiffTime)

test :: DiffTime -> Word32 -> Word32 -> IO Bool
test expected new old =
    return $ diffMSec32 new old == expected
          && diffMSec32 old new == -expected
  where
    diffMSec32 = systemClockDiffTime systemClock_GetTickCount

main :: IO ()
main = do
    True <- test 0 100 100
    True <- test 0 0 0
    True <- test 0 maxBound maxBound

    True <- test 0.001    101 100
    True <- test (-0.001) 100 101

    True <- test 2000000    3000000000 1000000000
    True <- test (-2000000) 1000000000 3000000000

    True <- test 0.001    minBound maxBound
    True <- test (-0.001) maxBound minBound

    putStrLn "All tests passed"
