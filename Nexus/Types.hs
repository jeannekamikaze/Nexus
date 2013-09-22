module Nexus.Types where

import Nexus.Actor

type UserName = String
type Password = String

-- | A message from a user to a client.
data NetworkMessage
    = Connect UserName    -- ^ Connect to the server with the given user name.
    | Disconnect          -- ^ Disconnect the client.
    | SendMessage String  -- ^ Send a message to all clients.
    | GetUserList         -- ^ Retrieve the list of clients.
    | ShutdownServer
    deriving (Eq, Show)

-- | A request from a client to the nexus.
data ClientRequest
    = Login UserName          -- ^ Log in as the given user.
    | Logoff UserName         -- ^ Log the given user off.
    | Send UserName String    -- ^ Send a message from the given sender to all clients.
    | RetrieveUserList        -- ^ Retrieve the list of clients.
    | Shutdown                -- ^ Shut the server down.
    deriving (Eq, Show)

-- | A reply from the nexus to a client.
data NexusReply
    = Ok                          -- ^ Generic acknowledgement.
    | UserAlreadyExists           -- ^ The request login failed because the user already exists.
    | UserLoggedIn UserName       -- ^ The given user has been logged in.
    | UserLoggedOff UserName      -- ^ The given user has been logged out.
    | UserList [UserName]         -- ^ The list of users.
    | MessageSent UserName String -- ^ The given message has been sent.
    | ServerShutDown              -- ^ The server has shut down.
    deriving (Eq, Show)

type ClientActor = Actor NexusReply
type NexusActor = Actor (ClientActor, ClientRequest)
