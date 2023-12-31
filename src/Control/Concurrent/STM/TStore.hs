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
    , unsafeLookupTStore
    , tStoreIndex
    -- * Reexports
    , module STM
    , Word32
    )
where


import Control.Concurrent
import Control.Exception (bracket,catch,SomeException(..))
import Control.Monad
import Data.Typeable
import Data.Word (Word32)
import GHC.Conc (STM,retry)
import qualified GHC.Conc as Unsafe (unsafeIOToSTM) 
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Foreign.Store as Store


-- | Parallel-safe store atop `Foreign.Store`.
newtype TStore a 
    = TStore { _runTStore :: Store.Store (TVar.TVar a) }


instance Eq (TStore a) where
    (==) ts0 ts1 =
        (==) 
            (_runStore $ _runTStore ts0) 
            (_runStore $ _runTStore ts1)


instance Show (TStore a) where
    show (TStore store) = 
        "TStore ("<>show store<>")"


newTStore :: a -> STM (TStore a)
newTStore a = do
    tv <- TVar.newTVar a
    ts <- _next
    __ <- _writeStore (_runTStore ts) tv
    pure ts


readTStore :: TStore a -> STM a
readTStore ts = do
    tvar <- _readStore (_runTStore ts)
    TVar.readTVar tvar


writeTStore :: TStore a -> a -> STM ()
writeTStore ts a = do 
    tvar <- _readStore (_runTStore ts)
    TVar.writeTVar tvar a


tStoreIndex :: TStore a -> Word32
tStoreIndex = 
    _runStore . _runTStore 


-- | Returns `Nothing` if it can't find a store for the given index. 
-- This function is unsafe in regards to the type of the `TStore`, 
-- since the store may be found, but it may be for another type.
unsafeLookupTStore :: Word32 -> STM (Maybe (TStore a))
unsafeLookupTStore n = do
    _lookupStore n >>= \case
        Nothing    -> pure Nothing
        Just store -> pure (Just (TStore store))


-------------------------------------------------------------------------------
-- INTERNALS ------------------------------------------------------------------
-------------------------------------------------------------------------------
-- | Index offset for the `Foreign.Store`.
_offset :: Word32
_offset = 
    let magicNumber = 67295 
    in (maxBound - magicNumber)
_nextCounter = do
    c <- Unsafe.unsafeIOToSTM do 
              catch 
                  (fmap Just (Store.readStore (_mkStore _offset)))
                  (\(_::SomeException) -> do
                      tvar <- TVar.newTVarIO (_offset + 10)
                      Store.writeStore (_mkStore _offset) tvar
                      Just <$> Store.readStore (_mkStore _offset))
    case c of
        Nothing -> retry
        Just var -> do
            n <- TVar.readTVar var 
            TVar.writeTVar var (1 + n)
            pure n
_writeStore s a = Unsafe.unsafeIOToSTM (Store.writeStore s a)
_readStore s = Unsafe.unsafeIOToSTM (Store.readStore s)
_lookupStore n = Unsafe.unsafeIOToSTM (Store.lookupStore n)
_mkStore = Store.Store
_devel k =
    let isDevelopment = False
    in if isDevelopment then k else pure ()
_runStore (Store.Store n) = n
_next :: STM (TStore a)
_next = do
    counter <- _nextCounter
    pure (TStore (_mkStore counter))
