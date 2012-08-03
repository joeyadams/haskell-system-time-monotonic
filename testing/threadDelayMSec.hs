import System.Time.Monotonic

main :: IO ()
main = do
    clock <- newClock
    print =<< clockGetTime clock

    threadDelayMSec $ MSec 500
    print =<< clockGetTime clock

    t0 <- clockGetTime clock
    threadDelayMSec $ MSec 10
    t1 <- clockGetTime clock
    threadDelayMSec $ MSec 20
    t2 <- clockGetTime clock
    threadDelayMSec $ MSec 1
    t3 <- clockGetTime clock
    print (t0, t1, t2, t3)

    putStrLn "Waiting -1 milliseconds (should do nothing)"
    threadDelayMSec $ MSec (-1)

    putStrLn "Waiting 105 seconds"
    threadDelayMSec $ MSec 105000
    print =<< clockGetTime clock

    putStrLn "Done."
