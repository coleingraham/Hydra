{-# LANGUAGE OverloadedStrings #-}

module Gfx.Rendering where

import           Control.Applicative
import           Control.Monad (unless)
import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L
import qualified Scripting.Lua as Lua
import           System.FilePath ((</>))
import           System.Exit (exitFailure)
import           System.IO
import qualified Util.GLFW as W
import           Util.State

initResources :: IO RenderNode
initResources = do
    GL.depthFunc $= Just GL.Less
    GL.blend $= GL.Enabled
    GL.blendEquation $= GL.FuncAdd
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
--    GL.blendFuncSeparate $= ((GL.SrcAlpha, GL.OneMinusSrcAlpha), (GL.One, GL.One))

    let attrib = GL.AttribLocation 0
    
    program <- U.simpleShaderProgram ("lib" </> "defaultShader.v.glsl") ("lib" </> "defaultShader.f.glsl")

    l <- Lua.newstate
    Lua.openlibs l
    -- disable harmful libs
    Lua.loadstring l "require = nil os = nil" "" 
    Lua.call l 0 0

    dc <- defaultGraphicState

    let rn = RenderNode program attrib l dc

    return rn

draw :: HydraState -> IO ()
draw state = do
    (width, height) <- GLFW.getFramebufferSize $ window state
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    drawNode state

drawNode :: HydraState -> IO ()
drawNode state = do
    clearMatrixStack $ graphic_state node
    GL.currentProgram $= (Just . U.program . shader_program $ node)
    U.enableAttrib (shader_program node) "coord3d"
    U.enableAttrib (shader_program node) "v_color"
    t <- maybe 0 id <$> GLFW.getTime
    Lua.getglobal l "_update"
    Lua.pushnumber l $ realToFrac t
    update_result <- Lua.pcall l 1 0 0
--    (if update_result /= 0 then print $ "Update Error: " ++ (show $ Lua.tostring l (-1)) )
    Lua.getglobal l "draw"
    draw_result <- Lua.pcall l 0 0 0
--    (if draw_result /= 0 then print $ "Draw Error: " ++ (show $ Lua.tostring l (-1)) )
    GL.vertexAttribArray (U.getAttrib (shader_program node) "coord3d") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib (shader_program node) "v_color") $= GL.Disabled
    return ()
    where
        node = nodes state
        prog = shader_program node
        l    = lua_state node

--------------------------------

color :: HydraState -> Double -> Double -> Double -> Double -> IO ()
color state r g b a = do
    writeIORef (draw_color $ graphic_state node) $ DrawColor rr gg bb aa
    where
        node = nodes state
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

translate :: HydraState -> Double -> Double -> Double -> IO ()
translate state x y z = do
    let mat = L.mkTransformationMat (L.identity :: L.M33 GL.GLfloat) $ L.V3 xx yy zz
    modifyActiveMatrix rn mat
    where
        rn = nodes state
        xx = realToFrac x :: GLfloat
        yy = realToFrac y :: GLfloat
        zz = realToFrac z :: GLfloat

rotate :: HydraState -> Double -> Double -> Double -> IO ()
rotate state x y z = do
    modifyActiveMatrix rn mat
    where
        rn  = nodes state
        xx  = realToFrac x :: GLfloat
        yy  = realToFrac y :: GLfloat
        zz  = realToFrac z :: GLfloat
        rx  = L.m33_to_m44 . L.fromQuaternion $ L.axisAngle (L.V3 1 0 0) xx
        ry  = L.m33_to_m44 . L.fromQuaternion $ L.axisAngle (L.V3 0 1 0) yy
        rz  = L.m33_to_m44 . L.fromQuaternion $ L.axisAngle (L.V3 0 0 1) zz
        mat = rx L.!*! ry L.!*! rz

rotate_ :: L.V3 GL.GLfloat -> HydraState -> Double -> IO ()
rotate_ axis state rad = do
    let mat = L.m33_to_m44 . L.fromQuaternion $ L.axisAngle axis angle
    modifyActiveMatrix rn mat
    where
        rn = nodes state
        angle = realToFrac rad :: GLfloat

rotateX :: HydraState -> Double -> IO ()
rotateX = rotate_ (L.V3 1 0 0)

rotateY :: HydraState -> Double -> IO ()
rotateY = rotate_ (L.V3 0 1 0)

rotateZ :: HydraState -> Double -> IO ()
rotateZ = rotate_ (L.V3 0 0 1)

scale :: HydraState -> Double -> Double -> Double -> IO ()
scale state x y z = do
   let mat = L.V4 (L.V4 xx 0 0 0) (L.V4 0 yy 0 0) (L.V4 0 0 zz 0) (L.V4 0 0 0 1)
   modifyActiveMatrix rn mat
    where
        rn = nodes state
        xx = realToFrac x :: GLfloat
        yy = realToFrac y :: GLfloat
        zz = realToFrac z :: GLfloat

cameraLocation :: HydraState -> Double -> Double -> Double -> IO ()
cameraLocation state x y z = do
    c <- readIORef $ camera gs
    writeIORef (camera gs) $ Camera vec (pan c) (tilt c) (roll c)
    where
        gs  = graphic_state $ nodes state
        vec = L.V3 xx yy zz
        xx  = realToFrac x :: GLfloat
        yy  = realToFrac y :: GLfloat
        zz  = realToFrac z :: GLfloat

cameraPan :: HydraState -> Double -> IO ()
cameraPan state p = do
    c <- readIORef $ camera gs
    writeIORef (camera gs) $ Camera (location c) pp (tilt c) (roll c)
    where
        gs  = graphic_state $ nodes state
        pp  = realToFrac p :: GL.GLfloat

cameraTilt :: HydraState -> Double -> IO ()
cameraTilt state t = do
    c <- readIORef $ camera gs
    writeIORef (camera gs) $ Camera (location c) (pan c) tt (roll c)
    where
        gs  = graphic_state $ nodes state
        tt  = realToFrac t :: GL.GLfloat

cameraRoll :: HydraState -> Double -> IO ()
cameraRoll state r = do
    c <- readIORef $ camera gs
    writeIORef (camera gs) $ Camera (location c) (pan c) (tilt c) rr
    where
        gs  = graphic_state $ nodes state
        rr  = realToFrac r :: GL.GLfloat

cameraView :: Camera -> Int -> Int -> L.M44 GL.GLfloat
cameraView c width height = projection L.!*! view
    where
        aspect     = fromIntegral width / fromIntegral height
        projection = U.projectionMatrix (pi/4) aspect 0.1 10
        cam        = U.pan (pan c) . U.roll (roll c)  .  U.tilt (tilt c) . U.dolly (location c) $ U.fpsCamera
        view       = U.camMatrix cam

stroke :: HydraState -> IO ()
stroke state = do
    writeIORef mode Stroke
    where
        mode = draw_mode $ graphic_state $ nodes state

fill :: HydraState -> IO ()
fill state = do
    writeIORef mode Fill
    where
        mode = draw_mode $ graphic_state $ nodes state

drawThing :: HydraState -> GL.PrimitiveMode -> GL.NumArrayIndices -> V.Vector Float -> IO ()
drawThing state mode num vertices = do
    t <- maybe 0 id <$> GLFW.getTime
    (width, height) <- GLFW.getFramebufferSize $ window state

    stack <- readIORef $ matrix_stack $ graphic_state rn
    cam <- readIORef $ camera $ graphic_state rn
    let mvp = (cameraView cam width height) L.!*! (flattenMatrix stack)
    U.asUniform mvp $ U.getUniform (shader_program rn) "mvp"

    clist <- getVertexColorList rn (V.length vertices)
    let colors = V.fromList clist
    V.unsafeWith colors $ \ptr -> do
            GL.vertexAttribPointer (U.getAttrib (shader_program rn) "v_color") $=
                (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 ptr)

    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer (U.getAttrib (shader_program rn) "coord3d") $=
          (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)

    GL.drawArrays mode 0 num
    where
        rn             = nodes state

drawLine :: HydraState -> 
    Double -> Double -> Double ->
    Double -> Double -> Double -> IO ()
drawLine state x1 y1 z1 x2 y2 z2 = do
    drawThing state GL.Lines 2 vertices
    where
        vertices = V.fromList [  realToFrac x1, realToFrac y1, realToFrac z1
                              ,  realToFrac x2, realToFrac y2, realToFrac z2
                              ] :: V.Vector Float

drawTriangle :: HydraState -> 
    Double -> Double -> Double ->
    Double -> Double -> Double ->
    Double -> Double -> Double -> IO ()
drawTriangle state x1 y1 z1 x2 y2 z2 x3 y3 z3 = do
    dmode <- readIORef mode
    drawThing state (whichMode dmode) 3 vertices
    where
        mode        = draw_mode $ graphic_state $ nodes state
        whichMode m = case m of
                       Fill   -> GL.Triangles 
                       Stroke -> GL.LineLoop
        vertices    = V.fromList [  realToFrac x1, realToFrac y1, realToFrac z1
                                 ,  realToFrac x2, realToFrac y2, realToFrac z2
                                 ,  realToFrac x3, realToFrac y3, realToFrac z3
                                 ] :: V.Vector Float

drawRectangle :: HydraState -> Double -> Double -> IO ()
drawRectangle state width height = do
    dmode <- readIORef mode
    drawThing state (whichMode dmode) 4 vertices
    where
        mode        = draw_mode $ graphic_state $ nodes state
        whichMode m = case m of
                       Fill   -> GL.Quads 
                       Stroke -> GL.LineLoop 
        x1 = (width/2) * (-1)
        x2 = (width/2)
        y1 = (height/2) * (-1)
        y2 = (height/2)
        z  = 0
        vertices = V.fromList [   realToFrac x1, realToFrac y1, realToFrac z
                               ,  realToFrac x2, realToFrac y1, realToFrac z
                               ,  realToFrac x2, realToFrac y2, realToFrac z
                               ,  realToFrac x1, realToFrac y2, realToFrac z
                               ] :: V.Vector Float

drawDisk :: HydraState -> Double -> Double -> IO ()
drawDisk state num rad = do
    dmode <- readIORef mode
    drawThing state (whichMode dmode) (floor n) vertices
    where
        mode        = draw_mode $ graphic_state $ nodes state
        whichMode m = case m of
                       Fill   -> GL.TriangleFan
                       Stroke -> GL.LineLoop 
        n        = realToFrac $ floor num :: Double
        f fn     = map (* rad) $ map fn $ map (*(pi*2)) $ map (/n) [0..(n-1)]
        xs       = f sin
        ys       = f cos
        zs       = take (floor n) $ repeat 0
        list     = concatMap (\(x,y,z) -> [x,y,z]) $ zip3 xs ys zs
        vertices = V.fromList $ map realToFrac list :: V.Vector Float

drawRing :: HydraState -> Double -> Double -> Double -> IO ()
drawRing state num rad thick = do
    dmode <- readIORef mode
    case dmode of
        Fill   -> drawThing state GL.QuadStrip (floor (n*2+2)) vertices
        Stroke -> do
                  drawDisk state num (rad + hthick)
                  drawDisk state num (rad - hthick)
    where
        mode        = draw_mode $ graphic_state $ nodes state
        hthick   = thick / 2
        n        = realToFrac $ floor num :: Double
        f fn r   = map (* r) $ map fn $ map (*(pi*2)) $ map (/n) [0..(n+1)]
        xs1      = f sin (rad+hthick)
        ys1      = f cos (rad+hthick)
        xs2      = f sin (rad-hthick)
        ys2      = f cos (rad-hthick)
        zs       = take (floor (n+1)) $ repeat 0
        outer    = zip3 xs1 ys1 zs
        inner    = zip3 xs2 ys2 zs
        list     = concatMap (\((x1,y1,z1),(x2,y2,z2)) -> [x1,y1,z1,x2,y2,z2]) $ zip outer inner
        vertices = V.fromList $ map realToFrac list :: V.Vector Float
