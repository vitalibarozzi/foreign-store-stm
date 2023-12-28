{-# LANGUAGE AllowAmbiguousTypes #-}
import Live
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad


main :: IO ()
main = do
    liveMonadIO do
        selfSwap ("foo", foo :: LiveMonad Int)
        liftIO $ forkIO $ forever $ do
            threadDelay 1000000 
            liveMonadIO (foo >> swapFoo >> foo)
    pure ()


foo :: (MonadIO m, Live m) => m Int
foo = do
    live ("foo",foo) do
        n <- pure 10
        liftIO (print "but not....")
        pure (1 + n)


-- TODO how do i make the ":r" also foo besides reloading the modules?
-- :def r (\args -> pure (":reaload\nswap (\"foo\",undefined) foo"))
-- 

swapFoo :: (MonadIO m, Live m) => m ()
swapFoo = do
    swap ("foo",foo) do
        liftIO (print "hacked by bar!!!!1")
        pure 666
    pure ()


swapQux :: (MonadIO m, Live m) => m ()
swapQux = do
    swap ("foo",foo) do
        liftIO (print "hacked by qux mfs!!!!1")
        pure 666
    pure ()
