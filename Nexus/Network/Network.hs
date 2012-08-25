module Nexus.Network.Network
where


import Control.Monad.Maybe
import Control.Monad.Trans.Class (lift)


-- | The network monad.
type Network = MaybeT IO


-- | Run the given network action.
runNetwork :: Network a -> IO (Maybe a)
runNetwork = runMaybeT


-- | Network the given IO action in the Network monad.
networkIO :: IO a -> Network a
networkIO = lift


-- | Yield the given result.
netYield :: Maybe a -> Network a
netYield x = MaybeT $ return x
