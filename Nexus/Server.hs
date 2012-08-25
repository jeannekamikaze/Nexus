module Nexus.Server
(
    startServer
)
where


import Nexus.Actor
import Nexus.ClientHandler
import Nexus.Messages
import Nexus.Network
import Nexus.NexusHandler
import Nexus.Types
import Nexus.User

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import System.IO (Handle)


-- | Start the server.
startServer :: Port -> IO ()
startServer port = serve port server


-- | The nexus thread.
server :: Server
server sock = do
    nexus <- newActor
    tid <- forkIO $ acceptClient sock nexus
    runActor (handleNexus newNexusServer) nexus
    killThread tid


-- | Accept new clients and authenticate them.
acceptClient :: Socket -> NexusHandler -> IO ()
acceptClient sock nexus = do
    client <- accept sock
    forkIO $ authenticateClient client nexus
    threadSleep 0.200
    acceptClient sock nexus


threadSleep :: Float -> IO ()
threadSleep = threadDelay . floor . (*10^6)
