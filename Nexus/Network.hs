module Nexus.Network
(
    module Nexus.Network.Auth
,   module Nexus.Network.Network
,   Server
,   Socket
,   Port
,   HostName
,   serve
,   Nexus.Network.accept
,   sendString
,   recvBString
,   recvString
,   tryRecvBString
)
where


import Nexus.Network.Auth
import Nexus.Network.Network

import qualified Data.ByteString.Char8 as B
import Network
import Network.Socket.ByteString
import System.IO (Handle, BufferMode(..), hSetBuffering)


-- | A network server.
type Server = Socket -> IO ()


type Port = Int


-- | Start a server on the given port.
serve :: Port-> Server -> IO ()
serve port server = withSocketsDo $ listenOn (PortNumber (fromIntegral port)) >>= server


-- | Accept a connection from an unauthed client.
accept :: Socket -> IO UnauthedClient
accept sock = do
    (handle, host, port) <- Network.accept sock
    hSetBuffering handle NoBuffering
    return $ unauthedClient handle host port


-- | Send a string.
sendString :: Handle -> String -> Network ()
sendString handle str = networkIO $ B.hPut handle $ B.pack str `B.append` B.pack"\r\n"


-- | Receive a bytestring.
recvBString :: Handle -> Network B.ByteString
recvBString handle = do
    str <- recvBString' handle $ B.pack ""
    netYield . Just $ str


recvBString' handle acc = do
    str <- networkIO $ B.hGetNonBlocking handle 512
    let acc' = acc `B.append` str
    if B.length acc' > 0 && B.last acc' == '\n'
        then
            let acc'' = B.init acc'
            in if B.length acc'' > 0 && B.last acc'' == '\r'
            then netYield (Just $ B.init acc'')
            else netYield (Just acc'')
        else recvBString' handle acc'


-- | Receive a string.
recvString :: Handle -> Network String
recvString = fmap B.unpack . recvBString


-- | Receive a bytestring.
-- 
-- This operation is non-blocking.
tryRecvBString :: Handle -> Network B.ByteString
tryRecvBString handle = do
    str <- networkIO $ B.hGetNonBlocking handle 128
    if B.length str == 0 then netYield Nothing else netYield $ Just (removeNewline str)


removeNewline :: B.ByteString -> B.ByteString
removeNewline str =
    if B.length str > 0 && B.last str == '\n'
    then
        let str' = B.init str
        in if B.length str' > 0 && B.last str' == '\r'
        then B.init str'
        else str'
    else str
