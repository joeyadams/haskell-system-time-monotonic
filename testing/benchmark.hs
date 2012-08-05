import System.Time.Monotonic
import Criterion.Main

main :: IO ()
main = do
    clock <- newClock
    putStrLn $ "Using " ++ clockDriverName clock

    defaultMain
        [ bench "newClock"     newClock
        , bench "clockGetTime" (clockGetTime clock)
        ]
