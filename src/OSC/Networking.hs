module OSC.Networking where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as BC
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

receiveFunc :: HydraState -> Message -> IO ()
receiveFunc h m
    | addr == "/code" = do
        Lua.loadstring ls (ascii_to_string $ d_ascii_string $ head msg) ""
        result <- Lua.pcall ls 0 0 1
        checkResult ls result
    | otherwise       = do
        Lua.getglobal ls "receiveOscMessage"
        Lua.pushstring ls $ BC.pack addr
        args ls
        Lua.pcall ls (numVals + 1) 0 0
        return ()
    where
        checkResult l r
            | r == 0 = return ()
            | otherwise = do
                e <- Lua.tostring l (-1)
                print $ "Receive Error: " ++ (show e)
                return ()
        addr      = messageAddress m
        msg       = messageDatum m
        ls        = lua_state $ nodes h
        args l    = mapM_ (pushVal l) msg
        numVals   = length msg
        pushVal l v = case datum_tag v of
                            's' -> do
                                Lua.pushstring l $ BC.pack $ ascii_to_string $ d_ascii_string v
                                return ()
                            'i' -> do
                                Lua.pushnumber l $ realToFrac $ d_int32 v
                                return ()
                            'f' -> do
                                Lua.pushnumber l $ realToFrac $ d_float v
                                return ()
                            'd' -> do
                                Lua.pushnumber l $ d_double v
                                return ()
