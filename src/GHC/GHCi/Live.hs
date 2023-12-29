{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Wrapper around `Foreign.Store` to make it thread-safe (hopefully).
module GHC.GHCi.Live 
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
    , indexTStore
    -- * Reexports
    , module STM
    )
where


import Control.DeepSeq
import Control.Monad
import qualified Control.Concurrent.STM as STM
import Data.Word (Word32)
import qualified Foreign.Store as Foreign (Store(Store),lookupStore,readStore,newStore)
import Control.Exception (bracket,catch,SomeException(..))
import GHC.Conc (STM,unsafeIOToSTM,retry)
import qualified Control.Concurrent.ReadWriteLock as Lock (RWLock, acquireRead, acquireWrite, releaseRead, releaseWrite, new)
import Data.IORef (IORef,newIORef,writeIORef,readIORef)
import Data.Typeable


-- | Read-only transactional store based on `Foreign.Store.Store`.
-- It is read-only so as to keep the mutable state inside the Store, 
-- under a lock.
newtype TStore a 
    = TStore 
        (Foreign.Store 
            ( Lock.RWLock
            , TypeRep
            --, (Typeable a => IORef a)
            , IORef a
            ))


instance Eq (TStore a) where
    {-# INLINE (==) #-}
    (==) !(TStore (Foreign.Store n0)) !(TStore (Foreign.Store n1)) = 
        n0 == n1


instance Show (TStore a) where
    {-# INLINE show #-}
    show !(TStore store) = 
        "TStore ("<>show store<>")"


newTStore :: (NFData a, Typeable a) => a -> STM (TStore a)
{-# INLINE newTStore #-}
newTStore !(_deepseq2 -> a) = do
    maybe retry pure =<< unsafeIOToSTM go
  where
    go = do
        catch 
            (fmap Just newTStoreIO) 
            (\(SomeException _) -> pure Nothing)
    newTStoreIO = do
        !lock  <- Lock.new
        !ioref <- newIORef a
        !store <- Foreign.newStore (lock, typeOf a, ioref)
        pure (TStore store)


readTStore :: (Typeable a) => TStore a -> STM a
{-# INLINE readTStore #-}
readTStore !(TStore store) = do
    ma <- unsafeIOToSTM do
        !(lock, typeR, spa) <- Foreign.readStore store
        bracket 
            (Lock.acquireRead lock >> pure lock) 
            (Lock.releaseRead)
            (\_ -> do 
                a <- (\x -> seq x x) <$> readIORef spa
                pure (if typeOf a == typeR then Just a else Nothing)
            )
    maybe retry pure ma


writeTStore :: (NFData a) => TStore a -> a -> STM ()
{-# INLINE writeTStore #-}
writeTStore !(TStore store) !(_deepseq2 -> a) = do 
    unsafeIOToSTM do
        !(lock, _typeR, spa) <- Foreign.readStore store
        bracket 
            (Lock.acquireWrite lock >> pure lock) 
            (Lock.releaseWrite)
            (const (writeIORef spa a))


-- | Will retry until if finds a store and a value of the expected type
-- inside it.
lookupTStore :: Word32 -> STM (TStore a)
{-# INLINE lookupTStore #-}
lookupTStore !n = do
    maybe retry pure =<< lookupTStore_ n


-- | Returns `Nothing` if it can't find a store for the given index
-- or if there are a type mismatch.
lookupTStore_ :: Word32 -> STM (Maybe (TStore a))
{-# INLINE lookupTStore_ #-}
lookupTStore_ !n = do
    unsafeIOToSTM (Foreign.lookupStore n) >>= \case
        Nothing    -> pure Nothing
        Just store -> pure (Just (TStore store))


-- | Get the corresponding index for the store.
indexTStore :: TStore a -> Word32
{-# INLINE indexTStore #-}
indexTStore !(TStore (Foreign.Store n)) = 
    seq n n 


-------------------------------------------------------------------------------
-- INTERNALS ------------------------------------------------------------------
-------------------------------------------------------------------------------

_deepseq2 :: NFData a => a -> a
{-# INLINE _deepseq2 #-}
_deepseq2 !a = deepseq a a
