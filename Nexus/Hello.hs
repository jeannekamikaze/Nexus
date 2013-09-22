module Nexus.Hello where

import Nexus.Actor

type Reply = String
data Request = Hello | Shutdown deriving Show

--hello :: ActorT IO (Actor Reply, Request) ()
hello :: ActorM (Actor Reply, Request) ()
hello = do
    (from, msg) <- recv
    case msg of
        Shutdown -> return ()
        Hello -> send "Hi there!" from >> hello
