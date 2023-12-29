{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Live 
 --   ( Live(..)
 --   , live
 --   , selfSwap
 --   , LiveMonad(..)
 --   , liveMonadIO
 --   )
where


import GHC.Conc
import Control.Monad
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Unsafe.Coerce
import Foreign.Store


test :: IO ()
test = do
    maybeStore <- lookupStore 1
    s <- case maybeStore of
        Just store -> readStore store
        Nothing    -> putStrLn "Creating new store..." >> newStore "" >> newStore "+" >>= readStore
    putStrLn s



live :: String -> IO a -> IO a
{-# NOINLINE live #-}
live name def = do
    maybeStore <- lookupStore 0
    case maybeStore of
        Just store -> do
            mpio <- readStore store
            case Map.lookup name mpio of
                Just io -> io
                Nothing -> do
                    writeStore store (Map.insert name def mpio)
                    live name def
        Nothing -> newStore (Map.singleton (name,def)) >> newStore (Map.singleton (name,def)) >> live name def



swap :: String -> IO a -> IO ()
swap name new = do
    maybeStore <- lookupStore 0
    case maybeStore of
        Just store -> do
            mpio <- readStore store
            writeStore store (Map.insert name new mpio)
            pure ()
        Nothing -> do

            store <- newStore new 
            pure ()

    

_imNew = undefined



{-
-- | For working with functions that can be hotswapped. Very unsafe. (Supports GHCi)
class Live m where
    -- | Hotswaps the definition under this name.
    swap :: Id (m a) -> m a -> m (Maybe a)
    -- | Returns `True` if caller under this name is the most recent.
    amInew :: Id (m a) -> m Bool
    -- | Run the most recent version of the function under this name, if none
    -- is found, run the one passed.
    runNew :: Id (m a) -> m a ->  m a


-- | Always runs the most recent definition under this name, case this could be
-- the most recent, it instead puts the definition under this name, then calls it.
live :: (Monad m, Live m) => (String, m a) -> m a -> m a
live (s,old) ma = do
    -- If I'm the new version of this function:
    amNew <- amInew (s,old)
    if amNew 
        -- I will run the action i have:
        then ma
        -- Otherwise I will run the newer version of it:
        else runNew (s,old) ma


selfSwap :: Applicative m => Live m => Id a -> m (Maybe a)
selfSwap (name,fn) = swap (name,undefined) (pure fn)


-------------------------------------------------------------------------------
-- EXAMPLE IMPLEMENTATION -----------------------------------------------------
-------------------------------------------------------------------------------

-- | Reader monad which has access to the internal `Foreign.Store.Store`.
--newtype LiveMonad a = MkLiveMonad (LiveMonad a -> LiveMonad a -> Bool, Config -> IO a)
newtype LiveMonad a = MkLiveMonad (Config -> IO a)

instance Functor LiveMonad where 
    fmap f (MkLiveMonad kio) = 
        MkLiveMonad (fmap (fmap f) kio)

instance Applicative LiveMonad where 
    pure = 
        MkLiveMonad . const . pure

instance Monad LiveMonad where 
    (>>=) io kio = 
        joinLiveMonad (fmap kio io)
      where
        joinLiveMonad :: LiveMonad (LiveMonad a) -> LiveMonad a
        joinLiveMonad (MkLiveMonad kio1) = do
            MkLiveMonad \tmap -> do
                MkLiveMonad kio2 <- kio1 tmap
                kio2 tmap

instance MonadIO LiveMonad where
    liftIO io = MkLiveMonad (const io)

instance Live LiveMonad where
    swap (name,_old) (MkLiveMonad kma) = 
        MkLiveMonad \tmapStore -> do
            tmap <- readStore tmapStore
            x <- atomically do
                mp <- readTVar tmap
                let newMap = Map.insert name (_newOpaque (unsafeCoerce kma)) mp
                writeTVar tmap newMap
                case Map.lookup name mp of
                    Just opq -> pure (Just ((_runOpaque opq) tmapStore))
                    Nothing -> pure Nothing
            case x of
                Nothing -> pure Nothing
                Just io -> io 
    amInew (name,_old) = 
        MkLiveMonad \tmapStore -> do
            tmap <- readStore tmapStore
            atomically do
                mp <- readTVar tmap
                case Map.lookup name mp of
                    Nothing -> pure True
                    Just __ -> pure False
    runNew (name,ma0b) ma1 =
        MkLiveMonad \tmapStore -> do
            tmap <- readStore tmapStore
            MkLiveMonad kma <- atomically do
                mp <- readTVar tmap
                case Map.lookup name mp of
                    Nothing -> do
                        let MkLiveMonad kio = ma1
                        writeTVar tmap (Map.insert name (_newOpaque (unsafeCoerce kio)) mp)
                        pure ma1
                    Just opq -> do
                        let ma0 = MkLiveMonad (\tmap -> (_runOpaque opq) tmap)
                        ma0bId <- identifyLiveMonad ma0b
                        ma0Id  <- identifyLiveMonad ma0
                        let imTheLastOne = ma0bId == ma0Id
                        if imTheLastOne
                            then pure ma1
                            else pure ma0
            kma tmapStore


-- very hacky
identifyLiveMonad :: LiveMonad a -> STM Int
identifyLiveMonad !(MkLiveMonad kio) = do
    let qqq = unsafeCoerce kio  -- TODO when refactoring this add deepseq
    pure qqq


liveMonadIO :: LiveMonad a -> IO a
liveMonadIO (MkLiveMonad kio) = do
    store <- maybe mkNewStore pure =<< lookupStore 0
    --print store
    kio store
  where
    mkNewStore = newStore =<< atomically (newTVar mempty)


-------------------------------------------------------------------------------
-- HELPERS --------------------------------------------------------------------
-------------------------------------------------------------------------------


type StrMap v = Map.Map String v
type Id ma = (String, ma)
type Config = Store (TVar (StrMap Opaque))


newtype Opaque = MkOpaque (forall a. a)
_runOpaque :: Opaque -> (forall a. Config -> IO a)
_runOpaque (MkOpaque a) = a 
_newOpaque :: (forall a. Config -> IO a) -> Opaque 
_newOpaque a = MkOpaque (unsafeCoerce a)
-}
