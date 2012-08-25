module Nexus.Messages
where


import Nexus.Types


-- | A message from a client.
data Message
    = Disconnect          -- ^ Disconnect the client.
    | SendMessage String  -- ^ Send a message to all authenticaetd clients.
    | GetUserList         -- ^ Retrieve the list of authenticated clients.
    | ShutdownServer
    deriving (Show)


-- | A request from a client to the nexus.
data Request
    = Login UserName Password -- ^ Log in as the given user.
    | Logoff UserName         -- ^ Log the given user off.
    | Send UserName String    -- ^ Send a message from the given sender to all authenticated clients.
    | RetrieveUserList        -- ^ Retrieve the list of authenticated clients.
    | Shutdown                -- ^ Shut the server down.
    deriving (Show)


-- | A reply from the nexus to a client 'Request'.
data Reply
    = LoginFailed             -- ^ The request login failed.
    | UserLoggedIn UserName   -- ^ The given user has been logged in.
    | UserLoggedOff UserName  -- ^ The given user has been logged out.
    | UserList [UserName]     -- ^ The list of authenticated users.
    | MessageSent UserName String -- ^ The given message has been sent.
    | ServerShutDown          -- ^ The server has shut down.
    deriving (Show)
