{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString as BS
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

registerLuaFunctions :: HydraState -> IO ()
registerLuaFunctions hs = do
    Lua.registerhsfunction l "backgroundRGBA" background
--    Lua.registerhsfunction l "colorRGBA"      (color hs)
    Lua.registerhsfunction l "line"           (drawLine hs)
    Lua.registerhsfunction l "triangle"       (drawTriangle hs)
    Lua.registerhsfunction l "rect"           (drawRectangle hs)
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

    Lua.registerhsfunction l "import" (import_file hs)
    
    Lua.loadfile l ("lib" </> "libHydra.hydra")
    Lua.call l 0 0
    where
        rn = nodes hs
        l = lua_state rn

data Options = Options {
     optPort     :: Int
    ,optSettings :: String
    }

defaultOptions :: Options
defaultOptions = Options {
     optPort     = 57150
    ,optSettings = ""
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
          ]

main :: IO ()
main = do
    win <- W.initialize "Hydra"
    node <- initResources
    let state = HydraState win node
    registerLuaFunctions state

    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options {
          optPort = port
        , optSettings = settings
        } = opts
    
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
