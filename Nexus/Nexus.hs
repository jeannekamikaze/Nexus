module Nexus.Nexus
(
    Nexus
,   runNexus
)
where

import Nexus.Actor
import Nexus.Types

import Control.Concurrent (threadDelay)
import qualified Data.Map as M

import qualified Data.List as L (delete, notElem)

--- Pure API

data Nexus = Nexus
    { users    :: [UserName]
    }

-- | The empty nexus.
emptyNexus :: Nexus
emptyNexus = Nexus []

-- | Handle the request.
handle :: Nexus -> ClientRequest -> (NexusReply, Nexus)

handle nexus (Login user) =
    if L.notElem user $ users nexus
    then (UserLoggedIn user, nexus { users = user : users nexus })
    else (UserAlreadyExists, nexus)

handle nexus (Logoff user) = (UserLoggedOff user, nexus { users = L.delete user $ users nexus })

handle nexus (Send sender msg) = (MessageSent sender msg, nexus)

handle nexus RetrieveUserList = (UserList (users nexus), nexus)

handle _ Shutdown = (ServerShutDown, emptyNexus)

--- Actor API

data State = State
    { stateNexus   :: Nexus
    , actorFromUser :: M.Map UserName ClientActor
    }

runNexus :: ActorM (ClientActor, ClientRequest) ()
runNexus = runNexus' (State emptyNexus M.empty)

runNexus' :: State -> ActorM (ClientActor, ClientRequest) ()
runNexus' state@(State nexus actorFromUser) = recv >>= \(sender, req) ->
    let (rep, nexus') = handle nexus req
        clientActors  = M.elems actorFromUser
        state'        = state { stateNexus = nexus' }
    in case rep of
        UserLoggedIn user -> do
            broadcast (sender : clientActors) rep
            runNexus' $ state' { actorFromUser = M.insert user sender $ actorFromUser }
        
        UserLoggedOff user -> do
            let actorFromUser' = M.delete user $ actorFromUser
            broadcast (M.elems actorFromUser') rep
            runNexus' $ state' { actorFromUser = actorFromUser' }
        
        MessageSent _ _ -> broadcast clientActors rep >> runNexus' state'
        
        ServerShutDown  -> broadcast clientActors rep >> runNexus' state'
        
        _ -> sender ! rep >> runNexus' state'
