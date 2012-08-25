Nexus
=====

Nexus is a little actor-based chat server written in Haskell.

Design
------

Every client communicates with a client handler, which is an actor that establishes a connection between the client
it handles and the nexus, which is another actor. Messages then naturally flow among clients, client handlers and the
nexus.

The actual logic driving client handlers and the nexus is pure, allowing one to quickcheck the protocol without
deploying the server on the network.

Features
--------

* A very simple Actor library inspired by Erlang and Cloud Haskell.
* Simple authentication mechanism.
* Sending and receiving messages.
* Retrieving the list of online users.


