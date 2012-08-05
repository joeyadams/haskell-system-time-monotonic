import System.Time.Monotonic
-- import Control.Concurrent   (forkOS)
import Control.Monad        (forever)

main :: IO ()
main = do
    clock <- newClock
    -- _ <- forkOS $ forever $ clockGetTime clock
    -- _ <- forkOS $ forever $ clockGetTime clock
    forever $ clockGetTime clock
