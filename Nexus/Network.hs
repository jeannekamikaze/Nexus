{-# LANGUAGE ScopedTypeVariables #-}
module Nexus.Network
(
    -- * Network
    Network
,   Socket
,   runNetwork
,   runNetwork'
,   netYield
,   sendString
,   sendBString
,   recvString
,   recvBString
,   tryRecvBString
,   closeSocket
,   networkIO
    -- * Server
,   Server
,   Client
,   Port
,   MaxClients
,   serve
,   accept
)
where

import Control.Exception (IOException , catch)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import qualified Data.ByteString.Char8 as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB

import Debug.Trace

import Control.Monad (when)

type Socket = N.Socket

data Disconnected = Disconnected deriving Show

data Network a = Network { run :: Socket -> EitherT Disconnected IO (Maybe a) }

instance Functor Network where

    fmap f (Network n) = Network $ \s -> (fmap . fmap) f $ n s
    
instance Monad Network where

    return x = Network . const . return . Just $ x
    
    (Network n) >>= f = Network $ \s -> do
        a <- n s
        case a of
            Nothing -> right Nothing
            Just x  -> run (f x) s

instance MonadIO Network where
    
    liftIO x = Network . const . fmap Just . liftIO $ x

-- Run the IO action in the Network monad.
networkIO :: IO a -> Network a
networkIO = liftIO

-- | Run a network action.
runNetwork :: Socket -> Network a -> IO (Either Disconnected (Maybe a))
runNetwork s (Network n) = runEitherT $ n s

-- | Run a network action within the network monad.
runNetwork' :: Network a -> Network (Either Disconnected (Maybe a))
runNetwork' n = Network $ \s -> liftIO (runNetwork s n) >>= return . Just

-- | Yield a result.
netYield :: Maybe a -> Network a
netYield = Network . const . right

-- | Send a string.
sendString :: Socket -> String -> Network Int
sendString sock str = networkIO $ do
    putStrLn $ "Sending string: " ++ str
    NB.send sock $ B.pack str `B.append` B.pack"\r\n"

-- | Send a bytestring.
sendBString :: Socket -> B.ByteString -> Network Int
sendBString sock str = networkIO $ NB.send sock $ str `B.append` B.pack"\r\n"

-- | Receive a string.
recvString :: Socket -> Int -> Network String
recvString sock nbytes = fmap B.unpack $ recvBString sock nbytes

-- | Receive a bytestring.
recvBString :: Socket -> Int -> Network B.ByteString
recvBString sock nbytes = do
    str <- liftIO $ safeRecv sock nbytes
    when (B.null str) $ do liftIO $ putStrLn "recvBString null"
    if B.null str then netFail Disconnected else netYield . Just $ str

-- | Receive a bytestring.
-- 
-- This operation is non-blocking.
tryRecvBString :: Socket -> Int -> Network B.ByteString
tryRecvBString sock nbytes = do
    str <- networkIO $ safeRecv sock nbytes
    when (B.null str) $ do liftIO $ putStrLn "tryRecvBString null"
    if B.null str then netYield Nothing else netYield $ Just (removeNewline str)

removeNewline :: B.ByteString -> B.ByteString
removeNewline str =
    if B.length str > 0 && B.last str == '\n'
    then
        let str' = B.init str
        in if B.length str' > 0 && B.last str' == '\r'
        then B.init str'
        else str'
    else str

-- | Close the socket
closeSocket :: Network ()
closeSocket = Network $ \s -> fmap Just . liftIO $ N.close s

safeRecv :: Socket -> Int -> IO (B.ByteString)
safeRecv sock nbytes = catch (NB.recv sock nbytes) (\(e :: IOException) -> return $ B.pack "")

-- Throw an error inside the network monad.
netFail :: Disconnected -> Network a
netFail = trace "netFail" (Network . const . left) 

--- Server

type Client = Socket
type Port   = N.PortNumber
type MaxClients = Int
type Server = Socket -> IO ()

-- | Start a server on the given port.
serve :: Port -> MaxClients -> Server -> IO ()
serve port maxClients serve = N.withSocketsDo $ do
    addr <- N.inet_addr "127.0.0.1"
    addrinfos <- N.getAddrInfo
                     (Just $ N.AddrInfo [N.AI_PASSIVE] N.AF_INET N.Stream N.defaultProtocol (N.SockAddrInet port addr) Nothing)
                     Nothing
                     (Just . show $ port)
    let serveraddr = head addrinfos
    sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
    N.bindSocket sock (N.addrAddress serveraddr)
    N.listen sock maxClients
    serve sock

-- | Accept a connection from a client.
accept :: N.Socket -> IO Client
accept sock = do
    (sock, _) <- N.accept sock
    return sock
