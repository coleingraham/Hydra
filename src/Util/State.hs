module Util.State where

import           Control.Concurrent
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Vector.Storable as V
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import qualified Scripting.Lua as Lua

data DrawColor = DrawColor {
         r :: GL.GLfloat
        ,g :: GL.GLfloat
        ,b :: GL.GLfloat
        ,a :: GL.GLfloat
    }

data DrawMode = Stroke | Fill deriving (Eq)

data Camera = Camera {
         location :: L.V3 GL.GLfloat
        ,pan      :: GL.GLfloat
        ,tilt     :: GL.GLfloat
        ,roll     :: GL.GLfloat
    }

defaultCamera :: Camera
defaultCamera = Camera (L.V3 0 0 0) 0 0 0

data GraphicState = GraphicState {
         draw_color   :: IORef DrawColor
        ,draw_mode    :: IORef DrawMode
        ,matrix_stack :: IORef [L.M44 GL.GLfloat]
        ,camera       :: IORef Camera
    }

emptyMatrix = L.mkTransformationMat (L.identity :: L.M33 GL.GLfloat) $ L.V3 0 0 0

flattenMatrix :: [L.M44 GL.GLfloat] -> L.M44 GL.GLfloat
flattenMatrix stack = foldl (\acc x -> acc L.!*! x) emptyMatrix stack

modifyActiveMatrix :: RenderNode -> L.M44 GL.GLfloat -> IO ()
modifyActiveMatrix rn mat = do
    stack <- readIORef $ matrix_stack gs
    let h = head stack
    let t = tail stack
    writeIORef (matrix_stack gs) $ (h L.!*! mat):t
    where
        gs = graphic_state rn

clearMatrixStack :: GraphicState -> IO ()
clearMatrixStack gs = writeIORef (matrix_stack gs) $ [emptyMatrix]

pushMatrix :: GraphicState -> IO ()
pushMatrix gs = do
    modifyIORef stack (push emptyMatrix)
    where
        stack = matrix_stack gs
        push val list = val:list

popMatrix :: GraphicState -> IO ()
popMatrix gs = do
    modifyIORef stack tail
    where
        stack = matrix_stack gs

defaultGraphicState :: IO GraphicState
defaultGraphicState = do
        c <- newIORef $ DrawColor 1 1 1 1
        m <- newIORef $ Stroke
        mat <- newIORef $ [emptyMatrix]
        cam <- newIORef defaultCamera
        return $ GraphicState c m mat cam 

colorToFloatList :: DrawColor -> [GL.GLfloat]
colorToFloatList c = [r c,g c,b c,a c]

getVertexColorList :: RenderNode -> Int -> IO [GL.GLfloat]
getVertexColorList rn len = do
    dc <- readIORef $ draw_color $ graphic_state rn
    let clist = intercalate [] $ replicate len (colorToFloatList dc)
    return clist

data RenderNode = RenderNode {
        shader_program   :: U.ShaderProgram, --GL.Program,
        vertex_attribute :: GL.AttribLocation,
        lua_state        :: Lua.LuaState,
        graphic_state    :: GraphicState
    }

data HydraState = HydraState {
        window     :: GLFW.Window,
        nodes      :: RenderNode
    }

