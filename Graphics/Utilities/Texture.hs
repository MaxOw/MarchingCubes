module Graphics.Utilities.Texture (
    TextureSize(..), loadPNGTexture, createTexture
) where

import Codec.Image.PNG
import Data.Array.Storable (withStorableArray)
import Data.Maybe
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL

----------------------------------------------------------------------

data TextureSize =
     Size1D GLsizei
   | Size2D GLsizei GLsizei
   | Size3D GLsizei GLsizei GLsizei

----------------------------------------------------------------------

tupleToSize2D :: Integral a => (a, a) -> TextureSize
tupleToSize2D (w, h) = Size2D (fromIntegral w) (fromIntegral h)

loadPNGTexture :: FilePath -> IO TextureObject
loadPNGTexture filename = do
    res <- loadPNGFile filename
    case res of
        Left ems -> error ems
        Right img -> do
            let size = tupleToSize2D $ dimensions img
            let pf = if (hasAlphaChannel img) then RGBA else RGB
            withStorableArray (imageData img) $ \id ->
              createTexturePtr Texture2D RGBA' size pf UnsignedByte id

----------------------------------------------------------------------

createTexture :: Storable a
    => TextureTarget
    -> PixelInternalFormat
    -> TextureSize
    -> PixelFormat
    -> DataType
    -> Maybe [a]
    -> IO TextureObject
createTexture target ipf size pf dt dat = do
    ndata <- maybe (return nullPtr) newArray dat
    createTexturePtr target ipf size pf dt ndata

createTexturePtr :: Storable a
    => TextureTarget
    -> PixelInternalFormat
    -> TextureSize
    -> PixelFormat
    -> DataType
    -> Ptr a
    -> IO TextureObject
createTexturePtr target ipf size pf dt dat = do
    [tex] <- genObjectNames 1
    textureBinding target $= Just tex
    texture target $= Enabled
    textureData target ipf size pf dt dat
    textureBinding target $= Nothing
    return tex

textureData Texture1D ipf (Size1D x) pf dt dat = do
    texImage1D NoProxy 0 ipf (TextureSize1D x) 0 $ PixelData pf dt dat
    textureFilter Texture1D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture1D S $= (Repeated, Repeat)

textureData Texture2D ipf (Size2D x y) pf dt dat = do
    texImage2D Nothing NoProxy 0 ipf (TextureSize2D x y) 0 $ 
        PixelData pf dt dat
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

textureData Texture3D ipf (Size3D x y z) pf dt dat = do
    texImage3D NoProxy 0 ipf (TextureSize3D x y z) 0 $ 
        PixelData pf dt dat
    textureFilter Texture3D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture3D S $= (Repeated, Repeat)
    textureWrapMode Texture3D T $= (Repeated, Repeat)
    textureWrapMode Texture3D R $= (Repeated, Repeat)

