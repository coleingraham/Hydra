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
import           System.Exit (exitFailure)
import           System.IO
import qualified Util.GLFW as W
import           Util.State

displayWidth win = do
    mon <- GLFW.getWindowMonitor win
    case mon of
        Nothing -> return 1
        Just m -> do
            vmode <- GLFW.getVideoMode m
            case vmode of
                Nothing -> return 1
                Just vm -> return $ GLFW.videoModeWidth vm

hy_sin :: Double -> IO Double
hy_sin a = return $ sin a

registerLuaFunctions :: HydraState -> IO ()
registerLuaFunctions hs = do
    Lua.registerhsfunction l "background" background
    Lua.registerhsfunction l "color" (color rn)
    Lua.registerhsfunction l "line" (drawLine rn)
    Lua.registerhsfunction l "triangle" (drawTriangle rn)
    Lua.registerhsfunction l "rect" (drawRectangle rn)

    Lua.registerhsfunction l "setWindowPos" (GLFW.setWindowPos $ window hs)
    Lua.registerhsfunction l "setWindowSize" (GLFW.setWindowSize $ window hs)
    Lua.registerhsfunction l "displayWidth" (displayWidth $ window hs)
    
    Lua.registerhsfunction l "sin" hy_sin
    
    Lua.loadfile l ("lib" </> "libHydra.hydra")
    Lua.call l 0 0
    where
        rn = nodes hs
        l = lua_state rn

-- main :: IO ()
main = do
    win <- W.initialize "Hydra Window!"
    node <- initResources
    let state = HydraState win node
    registerLuaFunctions state
    server <- createOscServer 57150 $ receiveFunc $ state
    W.mainLoop (draw state) win
    W.cleanup win
    Lua.close $ lua_state node
    print "testing"
