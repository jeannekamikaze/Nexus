module Nexus.NexusHandler
(
    NexusServer
,   newNexusServer
,   handleNexus
)
where


import Nexus.Actor
import Nexus.Messages
import Nexus.Network
import Nexus.Nexus
import Nexus.Types

import Control.Concurrent (threadDelay)
import qualified Data.List as L (delete)
import qualified Data.Map as M


data NexusServer = NexusServer
    { nexus   :: Nexus
    , clients :: M.Map UserName (Actor Reply)
    }


newNexusServer :: NexusServer
newNexusServer = NexusServer (newNexus) M.empty


handleNexus :: NexusServer -> ActorM (Request, Actor Reply) ()
handleNexus nexusServer@(NexusServer nexus clients) = do
    actorIO $ threadSleep 0.150
    
    -- Handle requests.
    (req, sender) <- recv
    actorIO . putStrLn $ "Nexus> received request: " ++ show req
    let (rep, nexus') = handleReq nexus req
        clientActors  = M.elems clients
        nexusServer'  = nexusServer { nexus = nexus' }
    actorIO . putStrLn $ "Nexus> reply is: " ++ show rep
    case rep of
        LoginFailed -> send rep sender >> handleNexus nexusServer'
        
        UserLoggedIn user -> do
            broadcast rep $ sender : clientActors
            handleNexus $ nexusServer' { clients = M.insert user sender clients }
        
        UserLoggedOff user -> do
            broadcast rep clientActors
            handleNexus $ nexusServer' { clients = M.delete user clients }
        
        UserList _        -> send rep sender >> handleNexus nexusServer'
        
        MessageSent _ _   -> broadcast rep clientActors >> handleNexus nexusServer'
        
        ServerShutDown    -> broadcast rep clientActors >> handleNexus nexusServer'


threadSleep :: Float -> IO ()
threadSleep = threadDelay . floor . (*10^6)
