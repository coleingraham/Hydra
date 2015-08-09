module Util.State where

import           Control.Concurrent
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Scripting.Lua as Lua

data DrawColor = DrawColor {
        r :: GL.GLfloat,
        g :: GL.GLfloat,
        b :: GL.GLfloat,
        a :: GL.GLfloat
    } deriving (Show)

data DrawMode = Stroke | Fill deriving (Eq)

data GraphicState = GraphicState {
        draw_color  :: IORef DrawColor
--        draw_mode   :: IORef DrawMode
    }

defaultGraphicState :: IO GraphicState
defaultGraphicState = do
        c <- newIORef $ DrawColor 1 1 1 1
--        m <- newIORef $ Stroke
        return $ GraphicState c

colorToFloatList :: DrawColor -> [GL.GLfloat]
colorToFloatList c = [r c,g c,b c,a c]

getVertexColorList :: RenderNode -> Int -> IO [GL.GLfloat]
getVertexColorList rn len = do
    dc <- readIORef $ draw_color $ graphic_state rn
    let clist = intercalate [] $ replicate len (colorToFloatList dc)
    return clist

data RenderNode = RenderNode {
        shader_program   :: GL.Program,
        vertex_attribute :: GL.AttribLocation,
        lua_state        :: Lua.LuaState,
        graphic_state    :: GraphicState
    }

data HydraState = HydraState {
        window     :: GLFW.Window,
--        osc_server :: ThreadId,
        nodes      :: RenderNode
    }

