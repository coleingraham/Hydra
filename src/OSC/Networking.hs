module OSC.Networking where

import           Control.Concurrent
import           Control.Monad
import           Sound.OSC
import qualified Scripting.Lua as Lua
import           Util.State

data NetAddr = NetAddr { netAddrHostName :: String, netAddrPortNum :: Int, oscClient :: (IO UDP) }

createOscServer :: Int -> (Message -> IO ()) -> IO ThreadId
createOscServer listeningPort parseFunc = forkIO $ withTransport server f
    where
        server = udpServer "127.0.0.1" listeningPort
        f = forever loop
        loop = do
            receivedMessage <- recvMessage
            case receivedMessage of
                Nothing -> return ()
                Just m -> liftIO $ parseFunc m
--            liftIO $ print receivedMessage

newNetAddr :: String -> Int -> NetAddr
newNetAddr ip port = NetAddr ip port (createOscClient ip port)

sendOsc :: IO UDP -> Message -> IO ()
sendOsc client oscMessage = withTransport client (sendMessage oscMessage)

sendOscBundle :: IO UDP -> Bundle -> IO()
sendOscBundle client oscBundle = withTransport client (sendBundle oscBundle)

createOscClient :: String -> Int -> IO UDP
createOscClient ip sendingPort = openUDP ip sendingPort

sendNetAddrMsg :: NetAddr -> Message -> IO ()
sendNetAddrMsg n m = sendOsc (oscClient n) m

-- TODO: this needs to map over all nodes for all messages going to Lua
receiveFunc :: HydraState -> Message -> IO ()
receiveFunc h m
    | addr == "/code" = do
        Lua.loadstring ls (ascii_to_string $ d_ascii_string $ head msg) ""
        Lua.call ls 0 0
        return ()
    | otherwise       = print "random message"
    where
        addr = messageAddress m
        msg  = messageDatum m
        ls   = lua_state $ nodes h
