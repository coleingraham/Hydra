module Util.State where

import           Control.Concurrent.STM (TQueue)
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

data DrawMode = Stroke | Fill deriving (Eq)

get_draw_mode :: Lua.LuaState -> IO DrawMode
get_draw_mode l = do
    Lua.getglobal l "_HYDRA"
    mode <- get_num l "draw_mode"
    case mode of
        0 -> return Stroke
        1 -> return Fill

data Camera = Camera {
         location :: L.V3 GL.GLfloat
        ,pan      :: GL.GLfloat
        ,tilt     :: GL.GLfloat
        ,roll     :: GL.GLfloat
    }

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
    
data GraphicState = GraphicState {
--         draw_color   :: IORef DrawColor
--        ,draw_mode    :: IORef DrawMode
        matrix_stack :: IORef [L.M44 GL.GLfloat]
--        ,camera       :: IORef Camera
    }

import_file :: HydraState -> BC.ByteString -> IO ()
import_file state name = do
    Lua.loadfile l $ (BC.unpack name) ++ ".hydra"
    result <- Lua.pcall l 0 0 0
    checkResult l result
    where
        checkResult l r
            | r == 0 = return ()
            | otherwise = do
                e <- Lua.tostring l (-1)
                print $ "Import Error: " ++ (show e)
                return ()
        l = lua_state $ nodes state

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

pushMatrix :: HydraState -> IO ()
pushMatrix state = do
    modifyIORef stack (push emptyMatrix)
    where
        stack = matrix_stack $ graphic_state $ nodes state
        push val list = val:list

popMatrix :: HydraState -> IO ()
popMatrix state = do
    modifyIORef stack tail
    where
        stack = matrix_stack $ graphic_state $ nodes state

defaultGraphicState :: IO GraphicState
defaultGraphicState = do
        c <- newIORef $ DrawColor 1 1 1 1
        m <- newIORef $ Stroke
        mat <- newIORef $ [emptyMatrix]
        cam <- newIORef defaultCamera
--        return $ GraphicState c m mat cam 
        return $ GraphicState mat 

colorToFloatList :: DrawColor -> [GL.GLfloat]
colorToFloatList c = [r c,g c,b c,a c]

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

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show
  
data HydraState = HydraState {
        window     :: GLFW.Window,
        nodes      :: RenderNode,
        eventsChan :: TQueue Event
    }

