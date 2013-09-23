module Main where

import Nexus.Actor
import Nexus.Client
import Nexus.Network
import Nexus.Nexus

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever)

serverPort = 1717

main = monitor runNexus main'

main' nexus getResult = serve serverPort (server nexus getResult)

server nexus getResult sock = do
    forkIO $ acceptConnection nexus sock
    server' nexus getResult

server' nexus getResult = do
    result <- getResult
    case result of
        Nothing -> threadDelay (200*10^3) >> server' nexus getResult
        Just _ -> return ()

acceptConnection nexus sock = forever $ accept sock >>= launch . runClient nexus
