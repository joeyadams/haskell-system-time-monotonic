-- Bombard a 'Clock' with 'clockGetTime' calls, to make sure this does not
-- degrade its accuracy.

import System.Time.Monotonic
import Control.Concurrent       (forkOS)
import Control.Monad            (forever)

main :: IO ()
main = do
    clock <- newClock
    _ <- forkOS $ forever $ clockGetTime clock
    _ <- forkOS $ forever $ clockGetTime clock
    _ <- forkOS $ forever $ clockGetTime clock
    forever $ do
        print =<< clockGetTime clock
        _ <- getLine
        return ()
