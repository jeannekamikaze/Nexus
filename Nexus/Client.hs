{-# LANGUAGE TupleSections #-}
module Nexus.Client
(
    runClient
)
where

import Nexus.Actor
import Nexus.Network (Network, Socket, Client(..), netYield, sendString, recvBString
                     ,runNetwork, runNetwork', closeSocket)
import Nexus.Types

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as B

import Debug.Trace

--- Pure API

data User
    = Regular
    { name :: UserName -- ^ Return the user's name.
    }
    | Admin
    { name :: UserName
    }
    deriving (Show)

-- | Handle the User message.
handle :: User -> NetworkMessage -> Maybe ClientRequest
handle user Disconnect = Just $ Logoff (name user)
handle user (SendMessage msg) = Just $ Send (name user) msg
handle user GetUserList = Just $ RetrieveUserList
handle user@Admin{} ShutdownServer = Just Shutdown
handle user ShutdownServer = Nothing

--- Actor API

data State = State NexusActor (Maybe User)

runClient :: NexusActor -> Client -> ActorT NexusReply IO ()
runClient n c = do
    actor <- lift newActor
    lift . forkIO . void $ runNetwork c $ runActorT (listenClient (State n Nothing)) actor
    lift . forkIO . void $ runNetwork c $ runActorT (listenNexus (State n Nothing)) actor
    return () 

listenClient :: State -> ActorT NexusReply Network () 
listenClient state@(State nexus user) = do
    me <- self
    
    message <- lift . runNetwork' $ recvMessage
    
    trace ("User message: " ++ show message) $ return ()
    
    -- Send request to nexus if non-empty.
    -- Return 'True' if the client issued a disconnect message.
    (user', disconnected) <- case message of
        Left _ -> return (user, True)
        Right maybeMessage -> case maybeMessage of
            Nothing -> return (user, False)
            Just (Connect username) -> do
                nexus ! (me, Login username)
                return $ if username == "x123admin"
                    then (Just . Admin $ username, False)
                    else (Just . Regular $ username, False)
            Just msg ->
                case user of
                    Nothing -> return (Nothing, False)
                    Just u -> case handle u msg of
                        Nothing -> return (user, False)
                        Just req -> do
                            me <- self
                            nexus ! (me,req)
                            return (user, msg == Disconnect)
    
    -- Quit if the user sent a disconnect message.
    if disconnected
        then lift closeSocket >> case user of
            Just u  -> nexus !(me, Logoff $ name u)
            Nothing -> return ()
        else actorIO (threadSleep 0.200) >> listenClient (State nexus user')

-- Handle nexus replies.
listenNexus :: State -> ActorT NexusReply Network () 
listenNexus state@(State nexus user) = do
    reply <- recv
    trace ("Nexus reply: " ++ show reply) $ return ()
    lift $ case reply of
        Ok                       -> sendString "Ok"
        UserAlreadyExists        -> sendString "Nickname is already in use"
        (UserLoggedIn user)      -> sendString $ user ++ " connected"
        (UserLoggedOff user)     -> sendString $ user ++ " disconnected"
        (UserList users)         -> sendString $ unwords users
        (MessageSent sender msg) -> sendString $ sender ++ "> " ++ msg
        ServerShutDown           -> sendString "Server shutting down"
                                 >> closeSocket >> return 0
    listenNexus state

recvMessage :: Network NetworkMessage
recvMessage = recvBString 256 >>= parse . fmap B.unpack . B.words

parse :: [String] -> Network NetworkMessage
parse [] = netYield Nothing
parse ("login":user) = netYield . Just . Connect $ unwords user
parse ["quit"] = netYield $ Just Disconnect
parse ("send":msgs) = netYield . Just . SendMessage $ unwords msgs
parse ["get-user-list"] = netYield $ Just GetUserList
parse ["shutdown"] = netYield $ Just ShutdownServer
parse _ = netYield Nothing

threadSleep :: Float -> IO ()
threadSleep = threadDelay . floor . (*10^6)