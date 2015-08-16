module Util.State where

import           Control.Concurrent
import qualified Data.ByteString.Char8 as BC
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

get_draw_mode :: Lua.LuaState -> IO DrawMode
get_draw_mode l = do
    Lua.getglobal l "_HYDRA"
    mode <- get_string l "draw_mode"
    case mode of
        "stroke" -> return Stroke
        "fill"   -> return Fill

data Camera = Camera {
         location :: L.V3 GL.GLfloat
        ,pan      :: GL.GLfloat
        ,tilt     :: GL.GLfloat
        ,roll     :: GL.GLfloat
    }

defaultCamera :: Camera
defaultCamera = Camera (L.V3 0 0 0) 0 0 0

toGL :: Double -> GL.GLfloat
toGL n = realToFrac n :: GL.GLfloat

get_string :: Lua.LuaState -> String -> IO String
get_string l name = do
    Lua.pushstring l $ BC.pack name
    Lua.gettable l (-2)
    val <- Lua.tostring l (-1)
    Lua.pop l 1
    return $ BC.unpack val

get_num :: Lua.LuaState -> String -> IO Double
get_num l name = do
    Lua.pushstring l $ BC.pack name
    Lua.gettable l (-2)
    val <- Lua.tonumber l (-1)
    Lua.pop l 1
    return val

get_vec :: Lua.LuaState -> IO (L.V3 GL.GLfloat)
get_vec l = do
    x <- get_num l "x"
    y <- get_num l "y"
    z <- get_num l "z"
    return $ L.V3 (toGL x) (toGL y) (toGL z)
    
get_camera :: Lua.LuaState -> IO Camera
get_camera l = do
    Lua.getglobal l "_HYDRA"
    Lua.pushstring l $ BC.pack "camera"
    Lua.gettable l (-2)
    pan  <- get_num l "pan"
    tilt <- get_num l "tilt"
    roll <- get_num l "roll"
    Lua.pushstring l $ BC.pack "location"
    Lua.gettable l (-2)
    loc  <- get_vec l
    return $ Camera loc (toGL pan) (toGL tilt) (toGL roll)

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

get_draw_color :: Lua.LuaState -> IO DrawColor
get_draw_color l = do
    Lua.getglobal l "_HYDRA"
    Lua.pushstring l $ BC.pack "draw_color"
    Lua.gettable l (-2)

    r <- get_num l "r"
    g <- get_num l "g"
    b <- get_num l "b"
    a <- get_num l "a"

    return $ DrawColor (toGL r) (toGL g) (toGL b) (toGL a)

getVertexColorList :: HydraState -> Int -> IO [GL.GLfloat]
getVertexColorList state len = do
--    dc <- readIORef $ draw_color $ graphic_state rn
    dc <- get_draw_color l
    let clist = intercalate [] $ replicate len (colorToFloatList dc)
    return clist
    where
        rn = nodes state
        l  = lua_state $ nodes state

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

