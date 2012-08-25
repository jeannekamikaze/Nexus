Nexus
=====

Nexus is a little actor-based chat server written in Haskell.

Design
------

The nexus is an actor that manages the state of the chat server. The nexus receives requests and replies to them,
updating its internal state in the process.

Clients do not talk directly to the nexus, but to client handlers. A client handler is an actor that receives messages
from the client in hand and translates them to requests that are then forwarded to the nexus. Replies from the nexus
are then forwarded back to the client, so the client and the nexus are effectively in touch.

The actual logic driving client handlers and the nexus is pure, allowing one to quickcheck the protocol without
having to deploy the server on the network.

Features
--------

* A very simple Actor library inspired by Erlang and Cloud Haskell.
* Simple authentication mechanism.
* Sending and receiving messages.
* Retrieving the list of online users.


