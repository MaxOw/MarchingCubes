module Graphics.Utilities.RenderTo (
    RenderToTextureBuffer(..),
    createRenderToTexture, renderToTexture,

    RenderToVertexBuffer(..),
    createRenderToVertex, renderToVertex
) where

import Control.Monad
import Foreign.Ptr
import Graphics.Rendering.OpenGL

import Graphics.Utilities.Shader
import Graphics.Utilities.Texture

--------------------------------------------------------------------------------

data RenderToTextureBuffer = RenderToTextureBuffer
   { rtFramebuffer :: FramebufferObject
   , rtTarget      :: TextureTarget
   , rtSize        :: TextureSize
   , rtTexture     :: TextureObject }

--------------------------------------------------------------------------------

createRenderToTexture :: TextureTarget -> PixelInternalFormat -> TextureSize ->
                         PixelFormat -> IO RenderToTextureBuffer
createRenderToTexture target pif size pf = do
    tex <- createTexture target pif size pf Float (Nothing :: Maybe [GLuint])
    
    [fbo] <- genObjectNames 1
    
    bindFramebuffer Framebuffer $= Just fbo
    textureBinding target $= Just tex
    case target of
        Texture1D -> framebufferTexture1D Framebuffer (ColorAttachment 0) target tex 0
        Texture2D -> framebufferTexture2D Framebuffer (ColorAttachment 0) target tex 0
        Texture3D -> framebufferTexture3D Framebuffer (ColorAttachment 0) target tex 0 0
    
    st <- get $ framebufferStatus Framebuffer
    when (st /= FramebufferComplete) $ do
        error $ "Framebuffer fail: " ++ (show st)
    
    textureBinding target $= Nothing
    bindFramebuffer Framebuffer $= Nothing
    
    return (RenderToTextureBuffer fbo target size tex)

getSize (Size1D sx) = (sx, 1, 0)
getSize (Size2D sx sy) = (sx, sy, 0)
getSize (Size3D sx sy sz) = (sx, sy, sz)

glFloat f = fromIntegral f :: GLfloat
vertex' x y z = vertex $ Vertex3 (glFloat x) (glFloat y) (glFloat z)
texCoord' :: GLfloat -> GLfloat -> GLfloat -> IO ()
texCoord' x y z = texCoord $ TexCoord3 x y z

renderToTexture :: ProgramEnviroment -> RenderToTextureBuffer -> IO ()
renderToTexture progEnv (RenderToTextureBuffer fbo target size tex) = do
    let (sx, sy, sz) = getSize size
    bindFramebuffer Framebuffer $= Just fbo
    
    preservingAttrib [ViewportAttributes] $ do
        viewport $= (Position 0 0, Size sx sy)
        matrixMode $= Projection
        preservingMatrix $ do
            loadIdentity
            ortho2D (fromIntegral 1) 0 (fromIntegral 1) 0
            
            matrixMode $= Modelview 0
            clear [ColorBuffer, DepthBuffer]
            loadIdentity
            
            setProgramEnv progEnv
            case target of
                Texture1D -> renderPrimitive Quads $ do
                    texCoord' 0 0 0 >> vertex' 0 0 0
                    texCoord' 1 0 0 >> vertex' 1 0 0
                    texCoord' 1 1 0 >> vertex' 1 1 0
                    texCoord' 0 1 0 >> vertex' 0 1 0
                Texture2D -> renderPrimitive Quads $ do
                    texCoord' 0 0 0 >> vertex' 0 0 0
                    texCoord' 1 0 0 >> vertex' 1 0 0
                    texCoord' 1 1 0 >> vertex' 1 1 0
                    texCoord' 0 1 0 >> vertex' 0 1 0
                Texture3D -> renderPrimitive Quads $ mapM_ (\z -> do
                    framebufferTexture3D Framebuffer (ColorAttachment 0) 
                                         Texture3D tex 0 (fromIntegral z)
                    renderPrimitive Quads $ do
                        let zc = (fromIntegral $ sz - z) / (fromIntegral sz)
                        texCoord' 0 0 zc >> vertex' 0 0 0
                        texCoord' 1 0 zc >> vertex' 1 0 0
                        texCoord' 1 1 zc >> vertex' 1 1 0
                        texCoord' 0 1 zc >> vertex' 0 1 0) [0..sz]
    
    bindFramebuffer Framebuffer $= Nothing

--------------------------------------------------------------------------------

data RenderToVertexBuffer = RenderToVertexBuffer
   { rvBuffer        :: BufferObject
   , rvVertexArray   :: VertexArrayObject 
   , rvPrimitiveMode :: PrimitiveMode }

--------------------------------------------------------------------------------

createRenderToVertex :: PrimitiveMode -> IO RenderToVertexBuffer
createRenderToVertex pm = do
    [bo] <- genObjectNames 1
    
    bindBuffer ArrayBuffer $= Just bo
    bufferData ArrayBuffer $= (2*1024^2, nullPtr, StaticCopy)
    bindBuffer ArrayBuffer $= Nothing
    
    [vao] <- genObjectNames 1
    
    bindVertexArray $= Just vao
    bindBuffer ArrayBuffer $= Just bo
    vertexAttribPointer (AttribLocation 0) $= --Position
        (ToFloat, VertexArrayDescriptor 4 Float 32 nullPtr)
    vertexAttribPointer (AttribLocation 2) $= --Normal
        (ToFloat, VertexArrayDescriptor 4 Float 32 (plusPtr nullPtr 16))
    bindBuffer ArrayBuffer $= Nothing
    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribArray (AttribLocation 2) $= Enabled
    bindVertexArray $= Nothing
    
    return (RenderToVertexBuffer bo vao pm)

renderToVertex :: ProgramEnviroment -> RenderToVertexBuffer -> IO () -> IO GLuint
renderToVertex progEnv rv action = do
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    
    [query] <- genObjectNames 1
    discardRasterizer $ do
        setProgramEnv progEnv
        
        bindBufferBase TransformFeedbackBuffer 0 $ rvBuffer rv
        beginQuery TransformFeedbackPrimitivesWritten query
        beginTransformFeedback $ rvPrimitiveMode rv
        
        --Render action to record
        action
        
        endTransformFeedback
        endQuery TransformFeedbackPrimitivesWritten
    priNum <- get $ queryResult query
    deleteObjectNames [query]
    --putStrLn $ show priNum
    return priNum
    --return (bindVertexArray $= Just (rvVertexArray rv) >>
            --drawArrays Triangles 0 (fromIntegral priNum * 3))

