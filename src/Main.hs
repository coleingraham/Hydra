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
    Lua.registerhsfunction l "colorRGBA"      (color hs)
    Lua.registerhsfunction l "line"           (drawLine hs)
    Lua.registerhsfunction l "triangle"       (drawTriangle hs)
    Lua.registerhsfunction l "rect"           (drawRectangle hs)
    Lua.registerhsfunction l "disk"           (drawDisk hs)
    Lua.registerhsfunction l "ring"           (drawRing hs)

    Lua.registerhsfunction l "stroke" (stroke hs)
    Lua.registerhsfunction l "fill"   (fill hs)

    Lua.registerhsfunction l "cameraLocation" (cameraLocation hs)
    Lua.registerhsfunction l "cameraPan"      (cameraPan hs)
    Lua.registerhsfunction l "cameraTilt"     (cameraTilt hs)
    Lua.registerhsfunction l "cameraRoll"     (cameraRoll hs)
    
    Lua.registerhsfunction l "pushMatrix" (pushMatrix (graphic_state rn))
    Lua.registerhsfunction l "popMatrix"  (popMatrix (graphic_state rn))

    Lua.registerhsfunction l "translate" (translate hs)
    Lua.registerhsfunction l "rotate"    (rotate hs)
    Lua.registerhsfunction l "rotateX"   (rotateX hs)
    Lua.registerhsfunction l "rotateY"   (rotateY hs)
    Lua.registerhsfunction l "rotateZ"   (rotateZ hs)
    Lua.registerhsfunction l "scale"     (scale hs)

    Lua.registerhsfunction l "setWindowPosition" (GLFW.setWindowPos $ window hs)
    Lua.registerhsfunction l "setWindowSize"     (GLFW.setWindowSize $ window hs)
    Lua.registerhsfunction l "displayWidthI"     displayWidth
    Lua.registerhsfunction l "displayHeightI"    displayHeight
    Lua.registerhsfunction l "displayX"          displayX
    Lua.registerhsfunction l "displayY"          displayY
    
    Lua.loadfile l ("lib" </> "libHydra.hydra")
    Lua.call l 0 0
    where
        rn = nodes hs
        l = lua_state rn

-- main :: IO ()
main = do
    win <- W.initialize "Hydra"
    node <- initResources
    let state = HydraState win node
    registerLuaFunctions state
    server <- createOscServer 57150 $ receiveFunc $ state
    W.mainLoop (draw state) win
    W.cleanup win
    Lua.close $ lua_state node
