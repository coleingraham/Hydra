{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM     (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString as BS
import           Gfx.Instancing
import           Gfx.Rendering
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           OSC.Networking
import qualified Scripting.Lua as Lua
import           System.FilePath ((</>))
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO
import qualified Util.GLFW as W
import           Util.State

displayWidth :: Int -> IO Int
displayWidth index = do
    mons <- GLFW.getMonitors
    case mons of
        Nothing -> return 1
        Just mons' -> do
            let m = mons' !! index
            vmode <- GLFW.getVideoMode m
            case vmode of
                Nothing -> return 1
                Just vm -> return $ GLFW.videoModeWidth vm

displayHeight :: Int -> IO Int
displayHeight index = do
    mons <- GLFW.getMonitors
    case mons of
        Nothing -> return 1
        Just mons' -> do
            let m = mons' !! index
            vmode <- GLFW.getVideoMode m
            case vmode of
                Nothing -> return 1
                Just vm -> return $ GLFW.videoModeHeight vm

displayX :: Int -> IO Int
displayX index = do
    mons <- GLFW.getMonitors
    case mons of
        Nothing -> return 1
        Just mons' -> do
            let m = mons' !! index
            pos <- GLFW.getMonitorPos m
            return $ fst $ pos

displayY :: Int -> IO Int
displayY index = do
    mons <- GLFW.getMonitors
    case mons of
        Nothing -> return 1
        Just mons' -> do
            let m = mons' !! index
            pos <- GLFW.getMonitorPos m
            return $ snd $ pos

windowX :: HydraState -> IO Int
windowX state = do
    (x,y) <- GLFW.getWindowPos $ window state
    return x

windowY :: HydraState -> IO Int
windowY state = do
    (x,y) <- GLFW.getWindowPos $ window state
    return y

registerLuaFunctions :: HydraState -> IO ()
registerLuaFunctions hs = do
    Lua.registerhsfunction l "backgroundRGBA" background
    Lua.registerhsfunction l "depthClear"     depthClear 
--    Lua.registerhsfunction l "colorRGBA"      (color hs)
    Lua.registerhsfunction l "line"           (drawLine hs)
--    Lua.registerhsfunction l "line2"          (drawPrimLine hs)
    Lua.registerhsfunction l "triangle"       (drawTriangle hs)
    Lua.registerhsfunction l "rect"           (drawRectangle hs)
--    Lua.registerhsfunction l "rect2"          (drawPrimRect hs)
    Lua.registerhsfunction l "disk"           (drawDisk hs)
    Lua.registerhsfunction l "ring"           (drawRing hs)

    Lua.registerhsfunction l "pushMatrix" (pushMatrix hs)
    Lua.registerhsfunction l "popMatrix"  (popMatrix hs)

    Lua.registerhsfunction l "translateXYZ" (translate hs)
    Lua.registerhsfunction l "rotateXYZ"    (rotate hs)
    Lua.registerhsfunction l "rotateX"      (rotateX hs)
    Lua.registerhsfunction l "rotateY"      (rotateY hs)
    Lua.registerhsfunction l "rotateZ"      (rotateZ hs)
    Lua.registerhsfunction l "scaleXYZ"     (scale hs)

    Lua.registerhsfunction l "setWindowPosition" (GLFW.setWindowPos $ window hs)
    Lua.registerhsfunction l "setWindowSize"     (GLFW.setWindowSize $ window hs)
    Lua.registerhsfunction l "displayWidthI"     displayWidth
    Lua.registerhsfunction l "displayHeightI"    displayHeight
    Lua.registerhsfunction l "displayXI"         displayX
    Lua.registerhsfunction l "displayYI"         displayY
    Lua.registerhsfunction l "windowX"           (windowX hs)
    Lua.registerhsfunction l "windowY"           (windowY hs)

    Lua.registerhsfunction l "import" (import_file hs)
    
    Lua.loadfile l ("lib" </> "libHydra.hydra")
    Lua.call l 0 0
    where
        rn = nodes hs
        l = lua_state rn

data Options = Options {
     optPort     :: Int
    ,optSettings :: String
    ,optWidth    :: Int
    ,optHeight   :: Int
    }

defaultOptions :: Options
defaultOptions = Options {
     optPort     = 57150
    ,optSettings = ""
    ,optWidth    = 640
    ,optHeight   = 480
    }

options :: [OptDescr (Options -> IO Options)]
options = [
            Option "p" ["port"]
                (ReqArg
                    (\arg opt -> return opt {optPort = read arg})
                    "PORT" )
                "osc listen port"
          , Option "s" ["settings"]
                (ReqArg
                    (\arg opt -> return opt {optSettings = arg})
                    "FILE")
                "load settings file"
          , Option "w" ["width"]
                (ReqArg
                    (\arg opt -> return opt {optWidth = read arg})
                    "WIDTH")
                "set initial window width"
          , Option "h" ["height"]
                (ReqArg
                    (\arg opt -> return opt {optHeight = read arg})
                    "HEIGHT")
                "set initial window height"
          ]

main :: IO ()
main = do

    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options {
          optPort     = port
        , optSettings = settings
        , optWidth    = width
        , optHeight   = height
        } = opts
    
    eventsChan <- newTQueueIO :: IO (TQueue Event)

    let w = optWidth opts
    let h = optHeight opts
    putStrLn $ "Hydra: resolution " ++ (show w) ++ " x " ++ (show h)
    win <- W.withWindow w h "Hydra" $ \win -> do
        GLFW.setErrorCallback               $ Just $ W.errorCallback           eventsChan
--        GLFW.setWindowPosCallback       win $ Just $ W.windowPosCallback       eventsChan
--        GLFW.setWindowSizeCallback      win $ Just $ W.windowSizeCallback      eventsChan
--        GLFW.setWindowCloseCallback     win $ Just $ W.windowCloseCallback     eventsChan
--        GLFW.setWindowRefreshCallback   win $ Just $ W.windowRefreshCallback   eventsChan
--        GLFW.setWindowFocusCallback     win $ Just $ W.windowFocusCallback     eventsChan
--        GLFW.setWindowIconifyCallback   win $ Just $ W.windowIconifyCallback   eventsChan
--        GLFW.setFramebufferSizeCallback win $ Just $ W.framebufferSizeCallback eventsChan
        GLFW.setMouseButtonCallback     win $ Just $ W.mouseButtonCallback     eventsChan
        GLFW.setCursorPosCallback       win $ Just $ W.cursorPosCallback       eventsChan
        GLFW.setCursorEnterCallback     win $ Just $ W.cursorEnterCallback     eventsChan
        GLFW.setScrollCallback          win $ Just $ W.scrollCallback          eventsChan
        GLFW.setKeyCallback             win $ Just $ W.keyCallback             eventsChan
        GLFW.setCharCallback            win $ Just $ W.charCallback            eventsChan

    node <- initResources
    let state = HydraState win node eventsChan
    registerLuaFunctions state

    let p = optPort opts
    putStrLn $ "Hydra: listening on port " ++ (show p)
    server <- createOscServer p $ receiveFunc $ state
{-
    let s = optSettings opts
    print $ "Hydra: loading settings from " ++ s
    Lua.loadfile (lua_state $ nodes state) s
    Lua.pcall (lua_state $ nodes state) 0 0 0
-}
    W.mainLoop (draw state) win
    W.cleanup win
    Lua.close $ lua_state node
