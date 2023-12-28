{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Live where


import Control.Monad
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Unsafe.Coerce


-- TODO now it just misses the integration with Foreign.Store, so we can cross the values
-- across reloads.


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

-------------------------------------------------------------------------------
-- EXAMPLE IMPLEMENTATION -----------------------------------------------------
-------------------------------------------------------------------------------

newtype LiveMonad a = MkLiveMonad (Config -> IO a)
instance Functor LiveMonad where fmap f (MkLiveMonad kio) = MkLiveMonad (fmap (fmap f) kio)
instance Applicative LiveMonad where pure = MkLiveMonad . const . pure
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
        MkLiveMonad \tmap -> do
            x <- atomically do
                mp <- readTVar tmap
                let newMap = Map.insert name (_newOpaque (unsafeCoerce kma)) mp
                writeTVar tmap newMap
                case Map.lookup name mp of
                    Just opq -> pure (Just ((_runOpaque opq) tmap))
                    Nothing -> pure Nothing
            case x of
                Nothing -> pure Nothing
                Just io -> io 
    amInew (name,_old) = 
        MkLiveMonad \tmap -> do
            atomically do
                mp <- readTVar tmap
                case Map.lookup name mp of
                    Nothing -> pure True
                    Just __ -> pure False
    runNew (name,_old) ma =
        MkLiveMonad \tmap -> do
            MkLiveMonad kma <- atomically do
                mp <- readTVar tmap
                case Map.lookup name mp of
                    Nothing -> do
                        let MkLiveMonad kio = ma
                        writeTVar tmap (Map.insert name (_newOpaque (unsafeCoerce kio)) mp)
                        pure ma
                    Just opq -> pure (MkLiveMonad (\tmap -> (_runOpaque opq) tmap))
            kma tmap
liveMonadIO :: LiveMonad a -> IO a
liveMonadIO (MkLiveMonad kio) = do
    tvar <- atomically do newTVar mempty
    kio tvar

-------------------------------------------------------------------------------
-- HELPERS --------------------------------------------------------------------
-------------------------------------------------------------------------------


type StrMap v = Map.Map String v
type Id ma = (String, ma)
type Config = TVar (StrMap Opaque)


newtype Opaque = MkOpaque (forall a. a)
_runOpaque :: Opaque -> (forall a. Config -> IO a)
_runOpaque (MkOpaque a) = a 
_newOpaque :: (forall a. Config -> IO a) -> Opaque 
_newOpaque a = MkOpaque (unsafeCoerce a)
