module Nexus.Network.Auth
(
    UnauthedClient(..)
,   AuthedClient(..)
,   Authenticator
,   Timeout
,   withUnauthedClient
,   unauthedClient
,   authenticate
)
where


import Nexus.Network.Network
import Nexus.Sys.Timer
import Nexus.Types

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Maybe
import Network
import System.IO (Handle, hClose)


-- | An unauthenticated client.
data UnauthedClient = UnauthedClient
    { unauthedSock :: Handle
    , unauthedHost :: HostName
    , unauthedPort :: PortNumber
    }


-- | An authenticated client.
data AuthedClient a = AuthedClient
    { authedSock :: Handle
    , authedHost :: HostName
    , authedPort :: PortNumber
    , authedData :: a
    }


-- | A client authenticator.
type Authenticator a = MVar (AuthedClient a) -> UnauthedClient -> IO ()


-- | A timeout value in seconds.
type Timeout = Float


-- | Create an unauthenticated client.
unauthedClient :: Handle -> HostName -> PortNumber -> UnauthedClient
unauthedClient = UnauthedClient


-- | Authenticate the given client.
-- 
-- The authenticator should write 'True' to the given 'MVar' when the client successfully logs in.
-- 
-- The authentication process is terminated if it does not authenticate the client within 'timeout'
-- seconds.
authenticate :: UnauthedClient -> Timeout -> Authenticator a -> IO (Maybe (AuthedClient a))
authenticate client timeout auth = do
    (result, tid) <- do
        authed <- newEmptyMVar
        tid <- forkIO $ (auth authed client) >> return ()
        result <- waitAuth authed timeout
        return (result, tid)
    case result of
        Just authedClient -> return result
        Nothing -> do
            hClose $ unauthedSock client
            killThread tid
            return Nothing


-- | Waits for the given authenticator for the given amount of seconds.
waitAuth :: MVar (AuthedClient a) -> Timeout -> IO (Maybe (AuthedClient a))
waitAuth authed timeout = waitAuth' authed timeout 0


waitAuth' :: MVar (AuthedClient a) -> Timeout -> Timeout -> IO (Maybe (AuthedClient a))
waitAuth' authed timeout delayed = do
    threadDelay $ toMicro 0.150
    result <- tryTakeMVar authed
    case result of
        Just x -> return result
        Nothing ->
            let delayed' = delayed + 0.150
            in if delayed' >= timeout
               then return Nothing
               else waitAuth' authed timeout delayed'
        
    


-- | Interact with the unauthenticated client.
withUnauthedClient :: UnauthedClient -> (Handle -> a) -> a
withUnauthedClient client f = f $ unauthedSock client


-- | Interact with the authenticated client.
withAuthedClient :: AuthedClient a -> (Handle -> b) -> b
withAuthedClient client f = f $ authedSock client


toMicro = floor . (*10^6)
