module Nexus.Hello where

import Nexus.Actor

type Reply = String
data Request = Hello | Shutdown deriving Show

--hello :: ActorT (Actor Reply, Request) IO ()
hello :: ActorM (Actor Reply, Request) ()
hello = do
    (from, msg) <- recv
    case msg of
        Shutdown -> from ! "Bye!"
        Hello    -> from ! "Hi there!" >> hello
