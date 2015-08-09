{-# LANGUAGE OverloadedStrings #-}

module Gfx.Rendering where

import           Control.Applicative
import           Control.Monad (unless)
import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Scripting.Lua as Lua
import           System.Exit (exitFailure)
import           System.IO
import qualified Util.GLFW as W
import           Util.State

initResources :: IO RenderNode
initResources = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fs $= fsSource
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        exitFailure

    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs
    GL.attribLocation program "coord3d" $= GL.AttribLocation 0
    GL.attribLocation program "v_color" $= GL.AttribLocation 1
    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program
    
    let attrib = GL.AttribLocation 0

    l <- Lua.newstate
    Lua.openlibs l
    -- disable harmful libs
    Lua.loadstring l "require = nil os = nil" "" 
    Lua.pcall l 0 0 0

    dc <- defaultGraphicState

    let rn = RenderNode program attrib l dc

    return rn

draw :: HydraState -> IO ()
draw state = do
    GL.depthFunc $= Just GL.Less
    (width, height) <- GLFW.getFramebufferSize $ window state
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    drawNode $ nodes state

drawNode :: RenderNode -> IO ()
drawNode node = do
    GL.currentProgram $= Just prog
    t <- maybe 0 id <$> GLFW.getTime
    Lua.loadstring l ("rt = " ++ (show t))""
    Lua.call l 0 0
    Lua.getglobal l "draw"
    Lua.pcall l 0 0 0
    return ()
    where
        prog = shader_program node
        l    = lua_state node

--------------------------------

color :: RenderNode -> Double -> Double -> Double -> Double -> IO ()
color node r g b a = do
    writeIORef (draw_color $ graphic_state node) $ DrawColor rr gg bb aa
    where
        rr = realToFrac r :: GLfloat
        gg = realToFrac g :: GLfloat
        bb = realToFrac b :: GLfloat
        aa = realToFrac a :: GLfloat

background :: Double -> Double -> Double -> Double -> IO ()
background r g b a = do
    GL.clearColor $= GL.Color4 rr gg bb aa
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    where
        rr = realToFrac r :: GLfloat
        gg = realToFrac g :: GLfloat
        bb = realToFrac b :: GLfloat
        aa = realToFrac a :: GLfloat


drawLine :: RenderNode -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
drawLine rn x1 y1 z1 x2 y2 z2 = do
    clist <- getVertexColorList rn (V.length vertices)
    let colors = V.fromList clist

    GL.vertexAttribArray attrib $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer attrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
    V.unsafeWith colors $ \ptr -> do
            GL.vertexAttribPointer (GL.AttribLocation 1) $=
                (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 ptr)
    GL.drawArrays GL.Lines 0 2 -- 2 is the number of vertices
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled
    GL.vertexAttribArray attrib $= GL.Disabled
    where
        attrib = vertex_attribute rn
        vertices = V.fromList [  realToFrac x1, realToFrac y1, realToFrac z1
                              ,  realToFrac x2, realToFrac y2, realToFrac z2
                              ] :: V.Vector Float

--------------------------------

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
             "attribute vec3 coord3d;"
           , "attribute vec4 v_color;"
--           , "uniform mat4 mvp;"
           , "varying vec4 f_color;"
           , ""
           , "void main(void) { "
--           , "  gl_Position = mvp * vec4(coord3d, 1.0);"
           , "  gl_Position = vec4(coord3d, 1.0);"
           , "  f_color = v_color;"
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [
            "varying vec4 f_color;"
           , ""
           , "void main(void) { "
           , "  gl_FragColor = f_color;"
           , "}"
           ]

--------------------------------

