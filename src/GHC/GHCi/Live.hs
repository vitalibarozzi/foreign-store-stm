module GHC.GHCi.Live 
--    ( live
--    , swap
--    , rebindReload
--    , foo
--    )
where


import Data.Word (Word32)
import qualified Data.Map as Map
import qualified Foreign.Store as Store
import System.IO.Unsafe
import Data.Unique
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad

        
    
foo :: IO Int
foo = do
    live "foo" do
        print "++++"
        pure (0 :: Int)


-- TODO how do i make the ":r" also foo besides reloading the modules?
-- :def r (\args -> pure (":reaload\nswap (\"foo\",undefined) foo"))


-- | To be used inside GHCi with ":cmd", so the macro ":r" will reload as 
-- expected, but also swap the needed functions with their new version.
rebindReload :: IO String
rebindReload = do
    -- TODo remove this hardcoded foo from here
    pure ":def! r (const (pure \":reload\\nswap \\\"foo\\\" foo\"))"
    

live :: String -> IO a -> IO a
{-# NOINLINE live #-}
live name io = do
    io
{-
    unsafePerformIO do
        unique0 <- newUnique
        pure \name def -> do
            Store.lookupStore _storeNumber >>= \case
                Just store -> ddoo
                    mpio <- Store.readStore store
                    case Map.lookup name mpio of
                        Just (lastOne, io) -> do
                            case lastOne of
                                Nothing -> do
                                    Store.writeStore store (Map.insert name (Just unique0,def) mpio)
                                    def
                                Just unique1 -> 
                                   if unique1 == unique0
                                       then def
                                       else undefined
                            if lastOne == Just unique
                                then do
                                    --putStrLn "eq"
                                    io
                                else do
                                    --putStrLn "not eq"
                                    Store.writeStore store (Map.insert name (Just unique,def) mpio)
                                    def
                        Nothing -> do
                            Store.writeStore store (Map.insert name (Just unique0,def) mpio)
                            def
                Nothing -> do
                    newStore (Map.singleton (name,def)) 
                    live name def
                                    -}


{-# NOINLINE swap #-}
swap :: String -> IO a -> IO ()
swap = do
    unsafePerformIO do
        unique0 <- newUnique
        pure \name new -> do
            Store.lookupStore _storeNumber >>= \case
                Just store -> do
                    mpio <- Store.readStore store
                    Store.writeStore store (Map.insert name (Just unique0, new) mpio)
                    pure ()
                Nothing -> do
                    _store <- newStore name (unique0, new)
                    pure ()


newStore :: String -> (Unique, IO a) -> IO (Store.Store (Map.Map String (Maybe Unique, IO a)))
newStore name (unique,io) = do
    store <- Store.newStore (Map.insert name (Just unique, io) mempty)
    when (show store /= ("Store "<>show _storeNumber)) (error "fuen")
    pure store
    


{-# INLINE _storeNumber #-}
_storeNumber :: Word32
_storeNumber = 0 -- TODO this is going to fail at some point


-- | This test only works on ghci.
main :: IO ()
main = do
    putStrLn "starting main..."
    liftIO $ forkIO $ forever $ do
        threadDelay 500000 
        putStrLn "running loop..."
        foo 
        threadDelay 500000 
        swapFoo 
        threadDelay 500000 
        foo
    pure ()


swapFoo :: IO ()
swapFoo = do
    swap "foo" do pure (666 :: Int)
