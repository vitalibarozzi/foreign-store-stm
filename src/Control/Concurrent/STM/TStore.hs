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
import Control.Exception (catch,SomeException(..))
import Data.Word (Word32)
import GHC.Conc (STM,retry)
import qualified GHC.Conc as Unsafe (unsafeIOToSTM) 
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Foreign.Store as Store


-- | Parallel-safe store atop `Foreign.Store`.
newtype TStore a = TStore 
    { _runTStore :: Store.Store (TVar.TVar a) }


instance Eq (TStore a) where
    (==) ts0 ts1 =
        (==) 
            (_runStore (_runTStore ts0))
            (_runStore (_runTStore ts1))


instance Show (TStore a) where
    show (TStore store) = 
        "TStore ("<>show store<>")"


newTStore :: a -> STM (TStore a)
newTStore a = do
    tv <- TVar.newTVar a
    ts <- _next
    __ <- _writeStore (_runTStore ts) tv
    pure ts


-- | If the Store does not exist it will retry until it does.
readTStore :: TStore a -> STM a
readTStore ts = do
    _readStore (_runTStore ts) 
        -- >>= maybe retry TVar.readTVar
        >>= maybe undefined TVar.readTVar


-- | Creates the TStore before writing to it if the TStore does not exists
-- already.
writeTStore :: TStore a -> a -> STM ()
writeTStore ts a = do 
    _readStore (_runTStore ts) 
        >>= maybe 
                (_writeStore (_runTStore ts) =<< TVar.newTVar a)
                (flip TVar.writeTVar a)


-- | Returns `Nothing` if it can't find a store for the given index. 
-- This function is unsafe in regards to the type of the `TStore`, 
-- since the store may be found, but it may be for another type.
unsafeLookupTStore :: Word32 -> STM (Maybe (TStore a))
unsafeLookupTStore =
    fmap (fmap TStore) . _lookupStore


-- | Gather the index of the TStore.
tStoreIndex :: TStore a -> Word32
tStoreIndex = 
    _runStore . _runTStore 


-- | Constructs a TStore reference, not the real Store in memory.
tStore :: Word32 -> TStore a
tStore = 
    TStore . Store.Store


-------------------------------------------------------------------------------
-- INTERNALS ------------------------------------------------------------------
-------------------------------------------------------------------------------


_counterStore :: TStore a
_counterStore = 
    tStore _offset


_substore :: TStore a -> Store.Store a
_substore = 
    Store.Store . tStoreIndex


-- | Index offset for the `Foreign.Store`.
_offset :: Word32
_offset = 
    let magicNumber = 67295 
    in (maxBound - magicNumber)


_next :: STM (TStore a)
_next = do
    fmap tStore nextCounter
  where
    nextCounter = do
        maybe retry swapCounter =<< getCounterTVar 
      where
        swapCounter var = do
            n <- TVar.readTVar var 
            TVar.writeTVar var (1 + n)
            pure n
        getCounterTVar = do 
            let readStoreIO = do
                    threadDelay 1
                    fmap Just (Store.readStore (_substore _counterStore))
            let handleErr SomeException{} = do
                    threadDelay 1
                    tvar <- TVar.newTVarIO (10 + _offset)
                    ____ <- Store.writeStore (_substore _counterStore) tvar
                    fmap Just (Store.readStore (_substore _counterStore))
            Unsafe.unsafeIOToSTM (catch readStoreIO handleErr)


_writeStore :: Store.Store a -> a -> STM ()
_writeStore s a = 
    Unsafe.unsafeIOToSTM do
        threadDelay 1
        Store.writeStore s a


_readStore :: Store.Store a -> STM (Maybe a)
_readStore s = 
    Unsafe.unsafeIOToSTM do
        threadDelay 1
        catch 
            (Just <$> Store.readStore s) 
            (\(e::SomeException) -> putStrLn (show e) >> pure Nothing)


_lookupStore :: Word32 -> STM (Maybe (Store.Store a)) 
_lookupStore n = 
    Unsafe.unsafeIOToSTM do
        threadDelay 1
        catch 
            (Store.lookupStore n) 
            (\(_::SomeException) -> pure Nothing)


_runStore :: Store.Store a -> Word32
_runStore (Store.Store n) = 
    n
