{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Wrapper around `Foreign.Store` to make it thread-safe (hopefully).
module Control.Concurrent.STM.TStore
    ( -- * Transactional Store
      TStore
    -- * Creation
    , newTStore
    -- * Reading
    , readTStore
    -- * Writting
    , writeTStore
    -- * Utils
    , lookupTStore
    , tstoreIndex
    -- * Reexports
    , module STM
    )
where


import Control.Monad
import qualified Control.Concurrent.STM as STM
import Control.Concurrent
import Data.Word (Word32)
import qualified Foreign.Store as Store
import Control.Exception (bracket,catch,SomeException(..))
import GHC.Conc (STM,unsafeIOToSTM,retry)
import qualified Control.Concurrent.STM.TVar as TVar
import Data.Typeable
import Control.DeepSeq


-- | Read-only transactional store based on `Foreign.Store`.
-- It is read-only so as to keep the mutable state inside the Store.
newtype TStore a = TStore (NFData a => Store.Store (TVar.TVar a))


instance NFData a => Eq (TStore a) where
    {-# INLINE (==) #-}
    (==) (TStore (Store.Store n0)) !(TStore (Store.Store n1)) = 
        n0 == n1


instance NFData a => Show (TStore a) where
    {-# INLINE show #-}
    show (TStore store) = 
        "TStore ("<>show store<>")"


newTStore :: NFData a => a -> STM (TStore a)
{-# INLINE newTStore #-}
newTStore !a = do
    unsafeIOToSTM (putStrLn "newTStore")
    tvar <- TVar.newTVar (deepseq a a)
    TStore store <- next
    _____ <- _writeStore store tvar
    pure (TStore store)


readTStore :: NFData a => TStore a -> STM a
{-# INLINE readTStore #-}
readTStore (TStore store) = do
    unsafeIOToSTM (putStrLn "readTStore")
    tvar <- _readStore store
    a <- TVar.readTVar tvar
    deepseq a (pure a)


writeTStore :: NFData a => TStore a -> a -> STM ()
{-# INLINE writeTStore #-}
writeTStore (TStore store) !a = do 
    unsafeIOToSTM (putStrLn "writeTStore")
    tvar <- _readStore store
    TVar.writeTVar tvar (deepseq a a)


-- | Returns `Nothing` if it can't find a store for the given index
-- or if there are a type mismatches.
lookupTStore :: Word32 -> STM (Maybe (TStore a))
{-# INLINE lookupTStore #-}
lookupTStore !n = do
    _lookupStore n >>= \case
        Nothing    -> unsafeIOToSTM (putStrLn $ "lookupTStore Nothing "<>show n) >> pure Nothing
        Just store -> unsafeIOToSTM (putStrLn $ "lookupTStore Just "<>show n<>" "<> show store) >> pure (Just (TStore store))


next :: STM (TStore a)
{-# INLINE next #-}
next = do
    unsafeIOToSTM (putStrLn "next")
    counter <- _nextCounter
    pure (TStore (_mkStore counter))
    

tstoreIndex (TStore (Store.Store n)) = 
    n

-------------------------------------------------------------------------------
-- INTERNALS ------------------------------------------------------------------
-------------------------------------------------------------------------------


-- | Index offset for the `Foreign.Store`.
_offset :: Word32
{-# INLINE _offset #-}
_offset = maxBound - 67295
            

_nextCounter :: STM Word32
_nextCounter = do
    unsafeIOToSTM (putStrLn "_nextCounter")
    _tryReadCounter >>= \case
        Nothing -> retry
        Just var -> do
            n <- TVar.readTVar var 
            unsafeIOToSTM (print n) 
            TVar.writeTVar var (1 + n)
            pure n

_tryReadCounter = do
          counter <- unsafeIOToSTM do 
              putStrLn "_tryReadCounter"
              catch 
                  (fmap Just (Store.readStore (_mkStore _offset)))
                  (\(e::SomeException) -> do
                      putStrLn ("exception: "<>show e)
                      tvar <- TVar.newTVarIO (_offset + 10)
                      Store.writeStore (_mkStore _offset) tvar
                      Just <$> Store.readStore (_mkStore _offset)
                      )
          pure counter

_writeStore !s !a = unsafeIOToSTM (threadDelay 1 >> Store.writeStore s a)
_readStore !s = unsafeIOToSTM (threadDelay 1 >> Store.readStore s)
_lookupStore !n = unsafeIOToSTM (threadDelay 1 >> Store.lookupStore n)
_mkStore = Store.Store
