module GHC.GHCi.Live 
    ( live
    , swap
    )
where


import Data.Word (Word32)
import qualified Data.Map as Map
import qualified Foreign.Store as Store



-- TODO how do i make the ":r" also foo besides reloading the modules?
-- :def r (\args -> pure (":reaload\nswap (\"foo\",undefined) foo"))


live :: String -> IO a -> IO a
live name def = do
    Store.lookupStore _storeNumber >>= \case
        Just store -> do
            mpio <- Store.readStore store
            case Map.lookup name mpio of
                Just io -> io
                Nothing -> do
                    Store.writeStore store (Map.insert name def mpio)
                    live name def
        Nothing -> do
            Store.newStore (Map.singleton (name,def)) 
            --Store.newStore (Map.singleton (name,def))  -- TODO wtf this is crazy
            live name def
{-# INLINE live #-}


swap :: String -> IO a -> IO ()
swap name new = do
    Store.lookupStore _storeNumber >>= \case
        Just store -> do
            mpio <- Store.readStore store
            Store.writeStore store (Map.insert name new mpio)
            pure ()
        Nothing -> do
            _store <- Store.newStore new 
            pure ()
{-# INLINE swap #-}


_storeNumber :: Word32
_storeNumber = 0 -- TODO this is going to fail at some point
{-# INLINE _storeNumber #-}


