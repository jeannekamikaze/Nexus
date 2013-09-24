{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nexus.Actor
(
    -- * Actor monad class
    MonadActor(..)
,   sendl
,   broadcast
    -- * Actor launcher class
,   ActorLauncher(..)
    -- * Actor
,   Actor
,   newActor
    -- * Actor monad
,   ActorM
,   runActor
    -- * Actor monad transformer
,   ActorT
,   runActorT
    -- * GHCi helper functions
,   actorIO
,   ioActor
,   iosend
)
where

import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

--- Actor data type and functions

newtype Actor a = Actor { chan :: TQueue a }

newActor :: IO (Actor a)
newActor = fmap Actor $ atomically newTQueue

--- MonadActor class

class Monad m => MonadActor a m | m -> a where
    
    -- | Send a message to the actor.
    send :: Actor b -> b -> m ()
    
    -- | Syntatic sugar for 'send'.
    (!) :: Actor b -> b -> m ()
    (!) = send
    
    -- | Receive a message. This operation is blocking.
    recv :: m a
    
    -- | Try to receive a message. This operation is non-blocking.
    tryRecv :: m (Maybe a)
    
    -- | Return myself.
    self :: m (Actor a)

-- | Send a list of messages to an actor.
sendl :: MonadActor a m => Actor b -> [b] -> m ()
sendl actor = mapM_ (send actor)

-- | Broadcast a message to several actors.
broadcast :: MonadActor a m => [Actor b] -> b -> m ()
broadcast actors msg = mapM_ (flip send msg) actors

--- ActorLauncher class

class ActorLauncher a m | m -> a where
    
    -- | Launch the actor in a separate thread.
    launch :: m b -> IO (Actor a)
    
    -- | Monitor an actor.
    monitor :: m b -> (Actor a -> IO (Maybe b) -> IO c) -> IO c

--- Actor monad

-- Newtype this to avoid the IO from leaking out into the wild.
newtype ActorM a b = ActorM (ActorT a IO b)
    deriving (Functor, Applicative, Monad)

instance MonadActor a (ActorM a) where
    
    send actor msg = ActorM $ send actor msg
    
    recv = ActorM recv
    
    tryRecv = ActorM tryRecv
    
    self = ActorM self
    
instance ActorLauncher a (ActorM a) where
    
    launch (ActorM m) = launch m
    
    monitor (ActorM m) run = monitor m run

runActor :: ActorM a b -> Actor a -> IO b
runActor (ActorM m) = runActorT m

--- Actor monad transformer

newtype ActorT a m b = ActorT { runActorT :: Actor a -> m b }

instance Functor f => Functor (ActorT a f) where

    fmap f m = ActorT $ \a -> fmap f $ runActorT m a

instance Applicative f => Applicative (ActorT a f) where

    pure f = ActorT $ \_ -> pure f
    
    f <*> m = ActorT $ \a ->
        let mf = runActorT f a
            mb = runActorT m a
        in mf <*> mb

instance Monad m => Monad (ActorT a m) where

    return x = ActorT $ \_ -> return x
    
    m >>= f = ActorT $ \a -> do
        b <- runActorT m a
        runActorT (f b) a

instance MonadIO m => MonadIO (ActorT a m) where

    liftIO x = ActorT $ \_ -> liftIO x >>= \b -> return b

instance MonadIO m => MonadActor a (ActorT a m) where
    
    send actor msg = ActorT $ \_ -> do
        liftIO . atomically $ writeTQueue (chan actor) msg
        return ()
    
    recv = ActorT $ liftIO . atomically . readTQueue . chan
    
    tryRecv = ActorT $ liftIO . atomically . tryReadTQueue . chan
    
    self = ActorT return

instance MonadTrans (ActorT a) where
    
    lift = ActorT . const

instance ActorLauncher a (ActorT a IO) where
    
    launch m = newActor >>= \a -> (forkIO . void $ runActorT m a) >> return a
    
    monitor m run = newActor >>= \a -> do
        result <- newEmptyMVar
        let getResult = tryTakeMVar result
        forkIO $ runActorT m a >>= putMVar result
        run a getResult

-- | Lift an 'IO' action into the ActorT monad.
--
-- This is equivalent to 'liftIO'.
actorIO :: MonadIO m => IO a -> ActorT b m a
actorIO = liftIO

--- Functions so that we can play with actors in ghci

-- | An actor that shows all received messages in the standard output channel.
--
-- Send it the message "kill" to kill it.
ioActor :: Show a => ActorT a IO ()
ioActor = do
    msg <- recv
    actorIO $ threadDelay (100*1000) >> print msg
    when (show msg /= "kill") ioActor

-- | Send a message from the first actor to the second.
iosend :: Actor a -> b -> Actor b -> IO ()
iosend from msg to = flip runActor from $ to ! msg
