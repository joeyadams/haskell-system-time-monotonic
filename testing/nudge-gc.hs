-- Make sure the nudgePeriodically thread goes away when the clock is GCed.
import System.Time.Monotonic

import Control.Monad    (replicateM_)
import System.Mem       (performGC)

main :: IO ()
main = do
    clock <- newClock
    replicateM_ 4 newClock
    putStrLn "Created 5 clocks"
    _ <- getLine
    performGC
    putStrLn "Garbage collected all but one clock"
    _ <- getLine
    MSec s <- clockGetTime clock
    putStrLn $ "That's " ++ show s ++ "ms of your life that you'll never get back."
    return ()
