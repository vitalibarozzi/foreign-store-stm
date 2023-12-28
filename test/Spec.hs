import Live
import Control.Monad.IO.Class
main :: IO ()
main = do
    liveMonadIO (foo >> swapFoo >> foo)
    pure ()


foo :: (MonadIO m, Live m) => m Int
foo = do
    live ("foo",foo) do
        n <- pure 10
        liftIO (print "im just the normal foo as in the start.")
        pure (1 + n)


swapFoo :: (MonadIO m, Live m) => m ()
swapFoo = do
    swap ("foo",foo) do
        liftIO (print "hacked by bar!!!!1")
        pure 666
    pure ()
