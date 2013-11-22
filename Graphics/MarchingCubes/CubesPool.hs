module Graphics.MarchingCubes.CubesPool (
    CubeBuffer(..), BufferPool, createBufferPool,
    getBuffer, insertBuffer, insertEmpty
) where

import Prelude hiding (foldl1)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List

import Graphics.Rendering.OpenGL
import Graphics.Utilities

--------------------------------------------------------------------------------

type CPos = Vector3 GLint

data CubeBuffer = CubeBuffer
   { cbDist     :: Int --min (x,y,z) abs $ pos - cubePos
   , cbPos      :: CPos
   , cbBuffer   :: RenderToVertexBuffer }

defBuffer b = CubeBuffer
   { cbDist = 0
   , cbPos = Vector3 0 0 0
   , cbBuffer = b }

instance Eq CubeBuffer where
    (==) = on (==) cbDist
instance Ord CubeBuffer where
    compare = on compare cbDist

type BufferPool = ([CubeBuffer], [CubeBuffer])

--------------------------------------------------------------------------------

createBufferPool :: Int -> IO BufferPool
createBufferPool poolSize = do
    sequence . replicate poolSize . fmap defBuffer . createRenderToVertex $ Triangles
    >>= \e -> return ([], e)

{--
recalcBufferPool :: CPos -> BufferPool -> BufferPool
recalcBufferPool pos pool = (reverse . sort $ rccd, snd pool)
     where rccd = map (\(d, c) -> c { cbDist = d }) . zip dists . fst $ pool 
           dists = foldl1 max . map posDif . fst $ pool
           posDif c = (-) <$> pos <*> (cbPos c)
--}

getBuffer :: BufferPool -> (CubeBuffer, BufferPool)
getBuffer (bfs, ebfs) = 
    if null ebfs then (head bfs, (tail bfs, ebfs)) 
    else (head ebfs, (bfs, tail ebfs))

insertBuffer :: CubeBuffer -> BufferPool -> BufferPool
insertBuffer b (bfs, ebfs) = (insert b bfs, ebfs)

insertEmpty :: CubeBuffer -> BufferPool -> BufferPool
insertEmpty b (bfs, ebfs) = (bfs, b:ebfs)

