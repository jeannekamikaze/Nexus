module Main where

import Nexus.Actor
import Nexus.Client
import Nexus.Network
import Nexus.Nexus

serverPort = 1717
maxClients = 10

main = launch runNexus >>= serve serverPort maxClients . server

server nexus sock = accept sock >>= launch . runClient nexus >> server nexus sock
