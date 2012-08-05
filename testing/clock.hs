import System.Time.Monotonic
import Control.Monad (forever)

main :: IO ()
main = do
    clock <- newClock
    putStrLn $ "Using " ++ clockDriverName clock
    forever $ do
        print =<< clockGetTime clock
        _ <- getLine
        return ()
