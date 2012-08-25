{-# LANGUAGE TupleSections #-}
module Nexus.ClientHandler
(
    Client
,   NexusHandler
,   newClient
,   handleClient
,   authenticateClient
)
where


import Nexus.Actor
import Nexus.User
import Nexus.Messages
import Nexus.Network
import Nexus.Types

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (putMVar)
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L (find, intercalate, null)
import System.IO (Handle, hClose)


type NexusHandler = Actor (Request, Actor Reply)


data Client = Client
    { nexus  :: Actor (Request, Actor Reply)
    , client :: AuthedClient User
    }


-- | Create a new client.
newClient :: Actor (Request, Actor Reply) -> AuthedClient User -> Client
newClient = Client


-- | Handle the given client.
handleClient :: Client -> ActorM Reply ()
handleClient client@(Client nexus authedClient) = do
    let sock = authedSock authedClient
    
    -- Handle client messages.
    result <- actorIO . runNetwork $ do
        msg <- tryRecvMessage sock
        return $ handleMsg (authedData authedClient) msg
    
    -- Send requests to nexus if non-empty.
    -- Return 'True' if the client issued a disconnect message.
    disconnected <- case result of
        Nothing -> return False
        Just (reqs, user') ->
            if not . L.null $ reqs
            then do
                me <- self
                sendl (fmap (,me) reqs) nexus
                return $ case L.find isLogoff reqs of
                    Nothing -> False
                    Just _  -> True
            else return False
    
    -- Handle nexus replies.
    reply <- tryRecv
    actorIO . runNetwork $ case reply of
        Nothing                       -> return () 
        Just (UserLoggedIn user)      -> sendString sock $ user ++ " connected"
        Just (UserLoggedOff user)     -> sendString sock $ user ++ " disconnected"
        Just (UserList users)         -> sendString sock $ L.intercalate " " users
        Just (MessageSent sender msg) -> sendString sock $ sender ++ "> " ++ msg
        Just ServerShutDown           -> sendString sock $ "Server shutting down"
    
    -- Quit if the user sent a disconnect message.
    case disconnected of
        True  -> actorIO $ hClose sock
        False -> (actorIO $ threadSleep 0.200) >> case result of
            Nothing         -> handleClient client
            Just (_, user') -> handleClient $ (Client nexus $ authedClient { authedData = user' })




-- | Authenticate the given client.
authenticateClient :: UnauthedClient -> NexusHandler -> IO ()
authenticateClient client nexus = (authenticate client 10 $ authenticator nexus) >> return ()


-- | The client authenticator.
authenticator :: NexusHandler -> Authenticator User
authenticator nexus authed client = do
    runNetwork $
        sendString (unauthedSock client) "Welcome, client. Please log in, you have 10 seconds."
    actor <- newActor
    authenticator' nexus authed client actor

authenticator' nexus authed client actor = do
    let sock = unauthedSock client
        host = unauthedHost client
        port = unauthedPort client
    
    threadSleep 0.200
    
    result <- runNetwork $ recvUserPass sock
    case result of
        Nothing -> authenticator' nexus authed client actor
        Just (user, pass) -> do
            (_, success) <- flip runActor actor $ do
                send (Login user pass, actor) nexus
                reply <- recv
                return $ case reply of
                    UserLoggedIn _ -> True
                    _              -> False
            if success
                then do
                    let authedClient = AuthedClient sock host port $ newUser user
                    putMVar authed authedClient
                    runNetwork $ sendString sock "Login successful."
                    runActor (handleClient $ Client nexus authedClient) actor
                    return ()
                else do
                    runNetwork $ sendString sock "Invalid login."
                    authenticator' nexus authed client actor


-- | Receive a (user, password) pair from the client.
recvUserPass :: Handle -> Network (UserName, Password)
recvUserPass handle = do
    str <- recvBString handle
    let userpass = fmap B.unpack $ B.split ' ' str
    case length userpass of
        2 -> netYield . Just $ (head userpass, head . tail $ userpass)
        _ -> netYield Nothing


tryRecvMessage :: Handle -> Network Message
tryRecvMessage handle = do
    str <- tryRecvBString handle
    parse . fmap B.unpack . B.split ' ' $ str


parse :: [String] -> Network Message
parse [] = netYield Nothing
parse ["quit"] = netYield $ Just Disconnect
parse ["get-user-list"] = netYield $ Just GetUserList
parse ["shutdown"] = netYield $ Just ShutdownServer
parse ("send":msgs) = netYield . Just . SendMessage $ L.intercalate " " msgs
parse _ = netYield Nothing


isLogoff (Logoff _) = True
isLogoff _          = False


threadSleep :: Float -> IO ()
threadSleep = threadDelay . floor . (*10^6)
