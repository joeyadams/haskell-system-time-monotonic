import System.Time.Monotonic
import System.Time.Monotonic.Direct
import Control.Monad (forever)

main :: IO ()
main = do
    sclock <- getSystemClock
    case sclock of
        SomeSystemClock sc ->
            putStrLn $ "Using " ++ systemClockName sc
    clock <- newClockWithDriver sclock
    forever $ do
        print =<< clockGetTime clock
        _ <- getLine
        return ()
