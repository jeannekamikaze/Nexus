module Nexus.User
(
    User
,   newUser
,   newAdmin
,   handleMsg
,   username
,   isAdmin
)
where


import Nexus.Messages
import Nexus.Types


-- | A regular client who has authenticated themselves.
data User = User
    { name  :: UserName
    , admin :: Bool
    }
    deriving (Show)


-- | Create a new regular client.
newUser :: UserName -> User
newUser name = User name False


-- | Create a new server admin.
newAdmin :: UserName -> User
newAdmin name = User name True 


-- | Handle the unauthenticated client's messages.
handleMsg :: User -> Message -> ([Request], User)
handleMsg client Disconnect = ([Logoff (name client)], client)
handleMsg client (SendMessage msg) = ([Send (name client) msg], client)
handleMsg client GetUserList = ([RetrieveUserList], client)
handleMsg client ShutdownServer = if (admin client) then ([Shutdown], client) else ([], client)


-- | Get the user's name.
username :: User -> UserName
username = name


-- | Return 'True' if the given user is an admin, 'False' otherwise.
isAdmin :: User -> Bool
isAdmin = admin
