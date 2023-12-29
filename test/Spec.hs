{-# LANGUAGE AllowAmbiguousTypes #-}
import GHC.GHCi.Live (live,swap)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad




-- | This test only works on ghci.
ghciTest :: IO ()
ghciTest = do
    liftIO $ forkIO $ forever $ do
        threadDelay 1000000 
        foo 
        --swapFoo 
        foo
        --swapQux
        foo
    pure ()
        
    


foo :: IO Int
foo = do
    live "foo" do
        print "og foo"
        pure (0 :: Int)


swapFoo :: IO ()
swapFoo = do
    swap "foo" do
        print "hacked by bar!!!!1"
        pure (666 :: Int)


swapQux :: IO ()
swapQux = do
    swap "foo" do
        liftIO (print "hacked by qux mfs!!!!1")
        pure (10 :: Int)
