{-# LANGUAGE BangPatterns #-}
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
    , lookupTStore_
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
import qualified Control.Concurrent.ReadWriteLock as Lock
import qualified Control.Concurrent.STM.TVar as TVar
import Data.Typeable


-- | Read-only transactional store based on `Foreign.Store`.
-- It is read-only so as to keep the mutable state inside the Store, 
-- under a lock.
newtype TStore a 
    = TStore 
        (Store.Store 
            ( Lock.RWLock
            , TVar.TVar a -- We use an TVar here so we don't have to write to the store.
            ))


instance Eq (TStore a) where
    {-# INLINE (==) #-}
    (==) (TStore (Store.Store n0)) !(TStore (Store.Store n1)) = 
        n0 == n1


instance Show (TStore a) where
    {-# INLINE show #-}
    show (TStore store) = 
        "TStore ("<>show store<>")"


newTStore :: a -> STM (TStore a)
{-# INLINE newTStore #-}
newTStore a = do
    TStore store <- next
    tvar  <- TVar.newTVar a
    lock  <- unsafeIOToSTM Lock.new
    _____ <- _writeStore store (lock, tvar)
    pure (TStore store)


readTStore :: TStore a -> STM a
{-# INLINE readTStore #-}
readTStore (TStore store) = do
    (lock, tvar) <- _readStore store
    foo <- unsafeIOToSTM do
        lockAcquired <- Lock.tryAcquireRead lock
        if lockAcquired
            then pure (Just (lock, tvar))
            else pure Nothing
    case foo of
        Nothing -> retry
        Just (lock,tvar) -> do
            a <- TVar.readTVar tvar
            unsafeIOToSTM (Lock.releaseRead lock) 
            pure a


writeTStore :: TStore a -> a -> STM ()
{-# INLINE writeTStore #-}
writeTStore (TStore store) a = do 
    (lock, tvar) <- _readStore store
    lockAcquired <- unsafeIOToSTM (Lock.tryAcquireWrite lock)
    unless lockAcquired retry
    TVar.writeTVar tvar a 
    unsafeIOToSTM (Lock.releaseWrite lock)


-- | Will retry until if finds a store.
lookupTStore :: Word32 -> STM (TStore a)
{-# INLINE lookupTStore #-}
lookupTStore !n = do
    maybe retry pure =<< lookupTStore_ n


-- | Returns `Nothing` if it can't find a store for the given index
-- or if there are a type mismatches.
lookupTStore_ :: Word32 -> STM (Maybe (TStore a))
{-# INLINE lookupTStore_ #-}
lookupTStore_ !n = do
    _lookupStore n >>= \case
        Nothing    -> pure Nothing
        Just store -> pure (Just (TStore store))


next :: STM (TStore a)
{-# INLINE next #-}
next = do
    (lock, counterTVar) <- _readStore (_mkStore _offset)
    lockAcquired <- unsafeIOToSTM (Lock.tryAcquireRead lock)
    if not lockAcquired
        then next
        else do
            counter <- TVar.readTVar counterTVar
            let newCounter = (1 + counter)
            TVar.writeTVar counterTVar newCounter
            pure (TStore (_mkStore counter))
    


-------------------------------------------------------------------------------
-- INTERNALS ------------------------------------------------------------------
-------------------------------------------------------------------------------



-- | Index offset for the `Foreign.Store`.
_offset :: Word32
{-# INLINE _offset #-}
_offset = maxBound - 67295
            

_readStore s = unsafeIOToSTM (Store.readStore s)

_writeStore s a = unsafeIOToSTM (Store.writeStore s a)

_lookupStore n = unsafeIOToSTM (Store.lookupStore n)

_mkStore = Store.Store
