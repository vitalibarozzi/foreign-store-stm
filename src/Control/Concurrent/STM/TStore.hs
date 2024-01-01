-- | Wrapper around `Foreign.Store` to make it thread-safe (hopefully).
module Control.Concurrent.STM.TStore
    ( -- * Transactional Store
      TStore
    , newTStore
    , readTStore
    , writeTStore
    -- * Utils
    , unsafeLookupTStore
    , tStoreIndex
    , tStore
    -- * Reexports
    , module STM
    , Word32
    )
where


import Control.Concurrent
import Control.Monad
import Control.Exception (catch,SomeException(..))
import Data.Word (Word32)
import GHC.Conc (STM,retry)
import qualified GHC.Conc as Unsafe (unsafeIOToSTM) 
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Foreign.Store as Store


-- | Thread-safe (hopefully) store atop `Foreign.Store`.
newtype TStore a = TStore 
    { _runTStore :: Store.Store (TVar.TVar a) }


instance Eq (TStore a) where
    {-# INLINE (==) #-}
    (==) ts0 ts1 =
        (==) 
            (_runStore (_runTStore ts0))
            (_runStore (_runTStore ts1))


instance Show (TStore a) where
    {-# INLINE show #-}
    show (TStore store) = 
        "TStore ("<>show store<>")"


newTStore :: a -> STM (TStore a)
{-# INLINABLE newTStore #-}
newTStore a = do
    tv <- TVar.newTVar a
    ts <- _nextCounter
    __ <- _writeStore (_substore (tStore ts)) tv
    pure (tStore ts)


-- | If the Store does not exist it will retry until it does.
readTStore :: TStore a -> STM a
{-# INLINABLE readTStore #-}
readTStore ts = do
    _readStore (_runTStore ts) 
        >>= maybe retry TVar.readTVar


-- | Creates the TStore before writing to it if the TStore does not exists
-- already.
writeTStore :: TStore a -> a -> STM ()
{-# INLINABLE writeTStore #-}
writeTStore ts a = do 
    _readStore (_runTStore ts) 
        >>= maybe 
                (_writeStore (_runTStore ts) =<< TVar.newTVar a)
                (flip TVar.writeTVar a)


-- | Returns `Nothing` if it can't find a store for the given index. 
-- This function is unsafe in regards to the type of the `TStore`, 
-- since the store may be found, but it may be for another type.
unsafeLookupTStore :: Word32 -> STM (Maybe (TStore a))
{-# INLINE unsafeLookupTStore #-}
unsafeLookupTStore w32 = do
    fmap (fmap TStore) . _lookupStore $ w32


-- | Gather the index of the TStore.
tStoreIndex :: TStore a -> Word32
{-# INLINE tStoreIndex #-}
tStoreIndex = 
    _runStore . _runTStore 


-- | Constructs a TStore reference, but do not construct the TStore in memory.
tStore :: Counter -> TStore a
{-# INLINE tStore #-}
tStore = 
    TStore . Store.Store . _runCounter


-------------------------------------------------------------------------------
-- INTERNALS ------------------------------------------------------------------
-------------------------------------------------------------------------------


_counterStore :: TStore Counter
{-# INLINE _counterStore #-}
_counterStore = 
    tStore (UnsafeCounter _offset)


-- | Index offset for the `Foreign.Store`.
_offset :: Word32
{-# INLINE _offset #-}
_offset = 
    let magicNumber = 67295  -- TODO why can't I change this number?
    in (maxBound - magicNumber)


-- | Used to extract the index from the store.
_runStore :: Store.Store a -> Word32
{-# INLINE _runStore #-}
_runStore (Store.Store n) = 
    n


-- | Used to extract/run the store.
_substore :: TStore a -> Store.Store (TVar.TVar a)
{-# INLINE _substore #-}
_substore = 
    Store.Store . tStoreIndex


-------------------------------------------------------------------------------
-- IO HELPERS -----------------------------------------------------------------
-------------------------------------------------------------------------------


_writeStore :: Store.Store a -> a -> STM ()
{-# INLINABLE _writeStore #-}
_writeStore s a = do
    Unsafe.unsafeIOToSTM do
        threadDelay 1
        Store.writeStore s a


_readStore :: Store.Store a -> STM (Maybe a)
{-# INLINABLE _readStore #-}
_readStore s = do
    Unsafe.unsafeIOToSTM do
        threadDelay 1
        catch 
            (Just <$> Store.readStore s) 
            (\(e::SomeException) -> putStrLn ("_readStore: "<> show e) >> pure Nothing)


_lookupStore :: Word32 -> STM (Maybe (Store.Store a)) 
{-# INLINABLE _lookupStore #-}
_lookupStore n = do 
    Unsafe.unsafeIOToSTM do
        threadDelay 1
        catch 
            (Store.lookupStore n) 
            (\(e::SomeException) -> putStrLn ("_lookupStore: "<> show e) >> pure Nothing)


-------------------------------------------------------------------------------
-- COUNTER --------------------------------------------------------------------
-------------------------------------------------------------------------------


-- | A counter used to know the next position/index of the store.
newtype Counter = UnsafeCounter 
    { _runCounter :: Word32 }


_initialCounter :: Counter
{-# INLINE _initialCounter #-}
_initialCounter = 
    UnsafeCounter (10 + _offset)


_incrementCounter :: Counter -> Counter
{-# INLINE _incrementCounter #-}
_incrementCounter (UnsafeCounter w32) = 
    if 1 + w32 <= _offset
        then error "Index collision."
        else UnsafeCounter (1 + w32)


_nextCounter :: STM Counter
{-# INLINABLE _nextCounter #-}
_nextCounter = do
    co <- _getCounter
    __ <- _incCounter co
    pure co
  where
    _getCounter :: STM Counter
    _getCounter = do 
        mw <- Unsafe.unsafeIOToSTM do
            threadDelay 1
            catch 
                (Store.readStore (_substore _counterStore) >>= pure . Left)
                (\e -> do
                    __ <- putStrLn ("_nextCounter: "<> show (e :: SomeException))
                    tv <- TVar.newTVarIO _initialCounter
                    __ <- Store.writeStore (_substore _counterStore) tv
                    pure (Right _initialCounter))
        case mw of
            Left tvr -> TVar.readTVar tvr
            Right co -> pure co
    _incCounter :: Counter -> STM ()
    _incCounter co = do 
        tv <- Unsafe.unsafeIOToSTM do Store.readStore (_substore _counterStore)
        TVar.writeTVar tv (_incrementCounter co)
