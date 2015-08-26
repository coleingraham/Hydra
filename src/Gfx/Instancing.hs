module Gfx.Instancing where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Util.State (Camera, DrawColor)

data HyrdaResorces = HyrdaResorces {
         scenes    :: Map String Scene
        ,cameras   :: Map String Camera
        ,objects   :: Map String Object
        ,meshes    :: Map String Mesh
        ,materials :: Map String Material
        ,textures  :: Map String Texture
        ,shaders   :: Map String GL.Program 
    }

data Scene = Scene {
         backgroundColor :: DrawColor
        ,camera          :: Camera
        ,objectNames     :: [String]
    }

data Object = Object {
         loc      :: L.V3 GL.GLfloat
        ,rot      :: L.V3 GL.GLfloat
        ,scl      :: L.V3 GL.GLfloat
        ,parent   :: Maybe Object
        ,children :: [String]
        ,mesh     :: [String]
    }

data Mesh = Mesh {
         origin        :: L.V3 GL.GLfloat
        ,vertexData    :: GL.BufferObject
        ,vertexColors  :: GL.BufferObject
        ,textureCoords :: GL.BufferObject
        ,shader        :: String
    }

data Material = Material 

data Texture = Texture 

