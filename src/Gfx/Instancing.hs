module Gfx.Instancing where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.GLUtil as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           System.FilePath ((</>))
import           Util.State (Camera, DrawColor)

data HydraResorces = HydraResorces {
--         scenes    :: Map String Scene
--        ,cameras   :: Map String Camera
--        ,objects   :: Map String Object
         meshes    :: Map String Mesh
--        ,materials :: Map String Material
--        ,textures  :: Map String Texture
        ,shaders   :: Map String U.ShaderProgram
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
         vertexData    :: GL.BufferObject
        ,num           :: Int
--        ,vertexNormals :: GL.BufferObject
--        ,textureCoords :: GL.BufferObject
--        ,material      :: String
        ,shader        :: String
    }

data Material = Material {
         diffuse   :: DrawColor
        ,specular  :: DrawColor
        ,emmission :: Float
        ,texture   :: [String]
    }

data Texture = Texture 

data InstanceData = InstanceData {
         color :: L.V4 GL.GLfloat
        ,mat   :: L.M44 GL.GLfloat
    }

lineBuffer :: IO GL.BufferObject
lineBuffer = U.makeBuffer GL.ArrayBuffer verts
    where
        verts = [  0.0,  0.0,  0.0
                ,  1.0,  1.0,  1.0 ] :: [GL.GLfloat]

rectBuffer :: IO GL.BufferObject
rectBuffer = U.makeBuffer GL.ArrayBuffer verts
    where
        verts = [ -0.5, -0.5,  0.0
                ,  0.5, -0.5,  0.0
                ,  0.5,  0.5,  0.0
                , -0.5,  0.5,  0.0 ] :: [GL.GLfloat]

defaultHydraResorces :: IO HydraResorces
defaultHydraResorces = do
    ln <- lineBuffer
    rt <- rectBuffer
    let m = Map.fromList [("line", Mesh ln 2 "default"),("rect", Mesh rt 4 "default")]
    program <- U.simpleShaderProgram ("lib" </> "defaultShader.v.glsl") ("lib" </> "defaultShader.f.glsl")
    let s = Map.fromList [("default", program)]
    return $ HydraResorces m s

