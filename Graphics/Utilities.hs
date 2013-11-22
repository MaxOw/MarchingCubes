module Graphics.Utilities (
    module Graphics.Utilities.Shader,
    module Graphics.Utilities.Texture,
    module Graphics.Utilities.RenderTo,
    module Graphics.Utilities.Profiling,
    module Graphics.Utilities.Camera,
    module Graphics.Utilities.ViewFrustumCulling,

    scale', translate', --lookAt',
    red, green, blue 
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.Utilities.Shader
import Graphics.Utilities.Texture
import Graphics.Utilities.RenderTo
import Graphics.Utilities.Profiling
import Graphics.Utilities.Camera
import Graphics.Utilities.ViewFrustumCulling

--------------------------------------------------------------------------------

scale' :: GLfloat -> IO ()
scale' s = scale s s s

translate' :: GLfloat -> GLfloat -> GLfloat -> IO ()
translate' x y z = translate $ Vector3 x y z

--------------------------------------------------------------------------------

red :: Color3 GLfloat
red = Color3 1 0 0

green :: Color3 GLfloat
green = Color3 0 1 0

blue :: Color3 GLfloat
blue = Color3 0 0 1

