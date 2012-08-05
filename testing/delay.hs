import System.Time.Monotonic

main :: IO ()
main = do
    clock <- newClock
    print =<< clockGetTime clock

    delay 0.5
    print =<< clockGetTime clock

    t0 <- clockGetTime clock
    delay 0.01
    t1 <- clockGetTime clock
    delay 0.02
    t2 <- clockGetTime clock
    delay 0.001
    t3 <- clockGetTime clock
    print (t0, t1, t2, t3)

    putStrLn "Waiting -1 seconds (should do nothing)"
    delay (-1)

    putStrLn "Waiting 105 seconds"
    delay 105
    print =<< clockGetTime clock

    putStrLn "Done."
