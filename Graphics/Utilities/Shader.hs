{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Utilities.Shader (
    loadProgram,
    ProgramEnviroment(..), UniformSetting(..), 
    Sampler(..), setProgramEnv
) where

import Control.Monad
import Data.Maybe

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

type UniformName = String
data UniformSetting = forall a. Uniform a => UniformSetting UniformLocation a
data Sampler = Sampler UniformLocation TextureTarget TextureObject

setUniforms :: [UniformSetting] -> IO ()
setUniforms = mapM_ $ \(UniformSetting location value) -> 
    uniform location $= value

setSamplers :: [Sampler] -> IO ()
setSamplers = mapM_ setSampler . zip [0..]
    where setSampler (i, (Sampler location target tex)) = do
            activeTexture $= TextureUnit i
            textureBinding target $= Just tex
            uniform location $= TexCoord1 (fromIntegral i :: GLint)

data ProgramEnviroment = ProgramEnviroment {
    peProgram :: Program,
    peUniforms :: [UniformSetting],
    peSamplers :: [Sampler]
}

setProgramEnv :: ProgramEnviroment -> IO ()
setProgramEnv progEnv = do
    currentProgram $= Just (peProgram progEnv)
    setUniforms (peUniforms progEnv)
    setSamplers (peSamplers progEnv)

loadShader :: Shader s => String -> IO s
loadShader fn = do
    putStrLn $ "Loading shader: " ++ fn
    src <- readFile $ "Shaders/" ++ fn
    [s] <- genObjectNames 1

    shaderSource s $= ("#version 150 compatibility\n":(lines src))
    compileShader s
    cs <- get $ compileStatus s
    unless cs $ do
        log <- get $ shaderInfoLog s
        error $ "Unable to compile shader. \n" ++ log

    putStrLn "Shader loaded."
    return s

loadProgram :: String -> String -> Maybe String -> Maybe [String] -> IO Program
loadProgram vn fn gn attribs = do
    vp <- loadShader vn
    fp <- loadShader fn

    [prog] <- genObjectNames 1
    attachedShaders prog $= ([vp], [fp])

    when (isJust gn) $ do
        gp <- loadShader $ fromJust gn
        attachShader prog (gp :: GeometryShader)

    when (isJust gn) $ 
        setTransformFeedbackVaryings prog InterleavedAttribs $ fromJust attribs

    linkProgram prog
    ls <- get $ linkStatus prog
    unless ls $ do
        log <- get $ programInfoLog prog
        error $ "Unable to link program. \n" ++ log

    return prog


