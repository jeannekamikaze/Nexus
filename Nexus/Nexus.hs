module Nexus.Nexus
(
    Nexus
,   newNexus
,   handleReq
)
where


import Nexus.Messages
import Nexus.Types

import qualified Data.List as L (delete, elem)


data Nexus = Nexus
    { users :: [UserName]
    }


-- | Construct a new nexus.
newNexus :: Nexus
newNexus = Nexus []


-- | Handle the given request.
handleReq :: Nexus -> Request -> (Reply, Nexus)

handleReq nexus (Login user pass) =
    if user == pass && (not . L.elem user $ users nexus)
    then (UserLoggedIn user, nexus { users = user : users nexus })
    else (LoginFailed, nexus)

handleReq nexus (Logoff user) = (UserLoggedOff user, nexus { users = L.delete user $ users nexus })

handleReq nexus (Send sender msg) = (MessageSent sender msg, nexus)

handleReq nexus RetrieveUserList = (UserList (users nexus), nexus)

handleReq nexus Shutdown = (ServerShutDown, newNexus)
