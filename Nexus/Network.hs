{-# LANGUAGE ScopedTypeVariables #-}
module Nexus.Network
(
    -- * Network
    Network
,   NetworkT
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
,   serve
,   accept
)
where

import Control.Exception (IOException , catch)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import qualified Data.ByteString.Char8 as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB

type Socket = N.Socket

data Disconnected = Disconnected deriving Show

type Network = NetworkT IO

data NetworkT m a = NetworkT { run :: Socket -> EitherT Disconnected m (Maybe a) }

instance Monad m => Functor (NetworkT m) where

    fmap f (NetworkT n) = NetworkT $ \s -> (fmap . fmap) f $ n s

instance Monad m => Monad (NetworkT m) where

    return x = NetworkT . const . return . Just $ x
    
    (NetworkT n) >>= f = NetworkT $ \s -> do
        a <- n s
        case a of
            Nothing -> right Nothing
            Just x  -> run (f x) s

instance MonadIO m => MonadIO (NetworkT m) where
    
    liftIO x = NetworkT . const . fmap Just . liftIO $ x

eitherIO :: MonadIO m => IO a -> EitherT Disconnected m (Maybe a)
eitherIO m = lift . liftIO . fmap Just $ m

-- Run the IO action in the Network monad.
networkIO :: MonadIO m => IO a -> NetworkT m a
networkIO = NetworkT . const . fmap Just . liftIO

-- | Run a network action.
runNetwork :: Socket -> NetworkT m a -> m (Either Disconnected (Maybe a))
runNetwork s (NetworkT n) = runEitherT $ n s

-- | Run a network action within the network monad.
runNetwork' :: Monad m => NetworkT m a -> NetworkT m (Either Disconnected (Maybe a))
runNetwork' n = NetworkT $ \s -> fmap Just . lift $ runNetwork s n

-- | Yield a result.
netYield :: Monad m => Maybe a -> NetworkT m a
netYield = NetworkT . const . right

-- | Send a string.
sendString :: MonadIO m => String -> NetworkT m Int
sendString str = NetworkT $ \sock -> eitherIO $ NB.send sock $ B.pack str `B.append` B.pack"\r\n"

-- | Send a bytestring.
sendBString :: MonadIO m => B.ByteString -> NetworkT m Int
sendBString str = NetworkT $ \sock -> eitherIO . NB.send sock $ str `B.append` B.pack"\r\n"

-- | Receive a string.
recvString :: MonadIO m => Int -> NetworkT m String
recvString nbytes = fmap B.unpack $ recvBString nbytes

-- | Receive a bytestring.
recvBString :: MonadIO m => Int -> NetworkT m B.ByteString
recvBString nbytes = NetworkT $ \sock -> do
    str <- liftIO $ safeRecv sock nbytes
    if B.null str then left Disconnected else right . Just $ str

-- | Receive a bytestring.
-- 
-- This operation is non-blocking.
tryRecvBString :: MonadIO m => Int -> NetworkT m B.ByteString
tryRecvBString nbytes = NetworkT $ \sock -> do
    str <- liftIO $ safeRecv sock nbytes
    right $ if B.null str then Nothing else Just . removeNewline $ str

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
closeSocket :: MonadIO m => NetworkT m ()
closeSocket = NetworkT $ \s -> fmap Just . liftIO $ N.close s

safeRecv :: Socket -> Int -> IO (B.ByteString)
safeRecv sock nbytes = catch (NB.recv sock nbytes) (\(e :: IOException) -> return $ B.pack "")

--- Server

type Client = Socket
type Port   = N.PortNumber
type Server = Socket -> IO ()

-- | Start a server on the given port.
serve :: Port -> Server -> IO ()
serve port server = N.withSocketsDo $ do
    addr <- N.inet_addr "127.0.0.1"
    addrinfos <- N.getAddrInfo
                     (Just $ N.AddrInfo [N.AI_PASSIVE] N.AF_INET N.Stream N.defaultProtocol (N.SockAddrInet port addr) Nothing)
                     Nothing
                     (Just . show $ port)
    let serveraddr = head addrinfos
    sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
    N.bindSocket sock (N.addrAddress serveraddr)
    N.listen sock 1
    server sock

-- | Accept a connection from a client.
accept :: N.Socket -> IO Client
accept sock = fmap fst $ N.accept sock
