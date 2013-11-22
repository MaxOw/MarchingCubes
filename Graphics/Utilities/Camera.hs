module Graphics.Utilities.Camera (
    Camera(..), newCamera, updateCamera,
    camDir, cameraView,
    wrap
) where

import Graphics.Rendering.OpenGL

import Data.Vect.Float as Vect
import Data.Vect.Float.Instances
import Data.Vect.Float.OpenGL 

--------------------------------------------------------------------------------

data Camera = Camera
   { camFi      :: Float
   , camTeta    :: Float
   , camPos     :: Vec3
   , camUp      :: Vec3 }

newCamera :: Camera
newCamera = Camera
   { camFi   = 0
   , camTeta = 0
   , camPos  = zero 
   , camUp   = Vec3 0 1 0 }

--------------------------------------------------------------------------------

updateCamera :: Float -> Float -> Vec3 -> Float -> Camera -> Camera
updateCamera mx my pos dt (Camera fi teta _ up) = (Camera nFi nTeta pos up)
    where
        nFi = clamp (-pi/2 + sd) (pi/2 - sd) $ fi + my * 0.1 * dt
        nTeta = wrap (-pi) pi $ teta + mx * 0.1 * dt
        -- nPos = pos + dir * moveSpeed * dt
        sd = 0.001

camDir :: Camera -> Vec3
camDir (Camera fi teta _ _) = rotate3 (-teta) (Vec3 0 1 0) vf
    where vf = rotate3 fi (Vec3 1 0 0) (Vec3 0 0 (-1))

cameraView :: Camera -> IO ()
cameraView cam = lookAt' pos tv
    where tv = pos + (camDir cam)
          pos = camPos cam
 
--------------------------------------------------------------------------------

clamp minV maxV v = min (max v minV) maxV
wrap minV maxV v = v - (fromIntegral $ floor $ (v - minV)/d) * d
    where d = maxV - minV

lookAt' :: Vec3 -> Vec3 -> IO ()
lookAt' (Vec3 ex ey ez) (Vec3 cx cy cz) = 
    lookAt (Vertex3 (realToFrac ex) (realToFrac ey) (realToFrac ez))
           (Vertex3 (realToFrac cx) (realToFrac cy) (realToFrac cz))
           (Vector3 0 1 0)

