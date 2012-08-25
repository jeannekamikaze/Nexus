module Nexus.Actor
(
    Actor
,   ActorM(..)
,   newActor
,   launchActor
,   send
,   sendl
,   broadcast
,   recv
,   tryRecv
,   self
,   actorSTM
,   actorIO
)
where


import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad.IO.Class


newtype Actor a = Actor { chan :: TQueue a }


newtype ActorM a b = ActorM { runActor :: Actor a -> IO ((Actor a, b)) }


instance Monad (ActorM a) where
    
    return x = ActorM $ \a -> return (a, x)
    
    --(>>=) :: ActorM a b -> (b -> Actor a c) -> Actor a c
    
    m >>= f = ActorM $ \a -> do
        (a', x) <- runActor m a
        runActor (f x) a'


instance MonadIO (ActorM a) where
    
    liftIO x = ActorM $ \a -> (x >>= \b -> return (a,b))


instance Functor (ActorM a) where
    
    fmap f m = ActorM $ \a -> runActor m a >>= \(a',b) -> return (a', f b)




newActor :: IO (Actor a)
newActor = atomically newTQueue >>= return . Actor


launchActor :: ActorM a b -> Actor a -> IO ThreadId
launchActor m actor = forkIO $ runActor m actor >> return ()


send :: a -> Actor a -> ActorM b ()
send msg actor = actorSTM $ writeTQueue (chan actor) msg


sendl :: [a] -> Actor a -> ActorM b ()
sendl msgs actor = mapM_ (flip send actor) msgs


broadcast :: a -> [Actor a] -> ActorM b ()
broadcast msg = mapM_ (send msg)


recv :: ActorM a a
recv = ActorM $ \a -> do
    val <- atomically . readTQueue . chan $ a
    return (a, val)


tryRecv :: ActorM a (Maybe a)
tryRecv = ActorM $ \a -> do
    val <- atomically . tryReadTQueue . chan $ a
    return (a, val)


self :: ActorM a (Actor a)
self = ActorM $ \a -> return (a,a)


actorSTM :: STM a -> ActorM b a
actorSTM = liftIO . atomically


actorIO :: IO a -> ActorM b a
actorIO = liftIO
