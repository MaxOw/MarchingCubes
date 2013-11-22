import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.List
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit

import Data.Vect.Float as Vect
import Graphics.Utilities
import Graphics.MarchingCubes

data State = State 
    { stMouseX   :: Float
    , stMouseY   :: Float
    , stMove     :: Float
    , stTime     :: Int
    , stCam      :: Camera }

closest :: Vec3 -> Vec3 -> Ordering
closest v0 v1 = compare (mv [v0]) (mv [v1])
    where
        mv = sum . map abs . destructVec3

fls = 3

main :: IO ()
main = do
    initWnd
    
    pb <- profileBase
    prog <- loadProgram "draw.vert" "draw.frag" Nothing Nothing
    time <- get $ uniformLocation prog "time"
    triTexture <- get $ uniformLocation prog "triTexture"
    triTextureNormal <- get $ uniformLocation prog "triTextureNormal"
    terTexture <- get $ uniformLocation prog "terTexture"
    terTextureNormal <- get $ uniformLocation prog "terTextureNormal"
    tex <- loadPNGTexture "Resources/RockyCliff.png"
    texNorm <- loadPNGTexture "Resources/RockyCliffNormal.png"
    texTer <- loadPNGTexture "Resources/Terrain.png"
    texTerNorm <- loadPNGTexture "Resources/TerrainNormal.png"
    let progEnv = ProgramEnviroment prog []
                    [Sampler triTexture Texture2D tex
                    ,Sampler triTextureNormal Texture2D texNorm
                    ,Sampler terTexture Texture2D texTer
                    ,Sampler terTextureNormal Texture2D texTerNorm]
    
    cubesToRender <- newIORef ([] :: [CubeToRender])
    pool <- newIORef =<< createBufferPool 400
    rList <- newIORef $ [Vec3 x y z | x <- [(-fls)..fls], y <- [(-1)..2], z <- [(-fls)..fls]]
    rList $~ sortBy closest
    context <- cubesContext
    
    et <- get elapsedTime
    state <- newIORef $ State 0 0 0 et (newCamera { camFi = (-pi/2) })
    
    displayCallback $= display progEnv cubesToRender state pb
    idleCallback $= Just (idle cubesToRender context pool rList state pb)
    keyboardMouseCallback $= Just (keyboard pb)
    passiveMotionCallback $= Just (mouseMotion state)
    
    mainLoop

--idle :: IO ()
idle cubesToRender context pool rList state pb = do
    beginProf pb "Idle"
    st <- get state
    let newPos = camPos (stCam st) -- &+ vt&*(speed*(stMove st))
    et <- get elapsedTime
    let dt = (fromIntegral $ et - stTime st) * 0.001
    let cam = updateCamera (stMouseX st) (stMouseY st) newPos dt (stCam st)
    state $= st { stCam = cam }
    
    {-- Inside View Frustum
    (Size sw sh) <- get screenSize
    let aspect = (fromIntegral sw)/(fromIntegral sh)
    let vfr = viewFrustum cam 30 aspect 1 100
    
    let sl = 10
    let (vfMin, vfMax) = viewFrustumAABB cam 30 aspect 1 100
    let floorV = liftV (fromIntegral . floor)
    let (Vec3 xMin yMin zMin) = (floorV . maxV vfMin) (neg $ Vec3 sl sl sl)
    let (Vec3 xMax yMax zMax) = (floorV . minV vfMax) (Vec3 sl sl sl)
    let bct = sum . map (\v -> if isNothing (pointInsideViewFrustum vfr v) then 0 else 1)
    let ll = [Vec3 x y z | x <- [xMin..xMax], y <- [yMin..yMax], z <- [zMin..zMax]]
    putStrLn $ "Inside view frustum: " ++ (show $ length ll)
    -- Inside View Frustum --}
    
    let renderOne t = do
        l <- readIORef rList
        when (not . null $ l) $ do
            beginProf pb "CubesGen"
            p <- readIORef pool
            (c, np) <- createCubeFromPool context p (head l)
            writeIORef pool np
            writeIORef rList (tail l)
            modifyIORef cubesToRender $ \x -> maybe x (:x) c
            --modifyIORef cubesToRender ((++) (catMaybes [c]))
            endProf pb "CubesGen"
            nt <- get elapsedTime
            if (nt - t) < 20 then (renderOne t)
                else return ()
    
    tSt <- get elapsedTime
    renderOne tSt
    
    --threadDelay 1
    endProf pb "Idle"
    postRedisplay Nothing

--display :: Program -> [IO ()] -> IO ()
display progEnv cubesToRender state pb = do
    endProf pb "All"
    beginProf pb "All"
    endProf pb "Other"
    beginProf pb "Display"
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    
    st <- get state
    cameraView (stCam st)
    
    --(Size sw sh) <- get screenSize
    --let aspect = (fromIntegral sw)/(fromIntegral sh)
    --let vfr = viewFrustum (stCam st) 30 aspect 1 100
    
    {--
    translate' 0 0 (-8)
    scale' (1/(fromIntegral cubeSize))
    let cnt = -(fromIntegral cubeSize)/2
    translate' cnt cnt 0
    --}
    
    et <- get elapsedTime
    invSize <- get $ uniformLocation (peProgram progEnv) "invSize"
    time <- get $ uniformLocation (peProgram progEnv) "time"
    setProgramEnv (progEnv { peUniforms = 
        [UniformSetting time $ TexCoord1 ((fromIntegral et * 0.001) :: GLfloat)
        ,UniformSetting invSize $ TexCoord1 (1/(fromIntegral cubeSize) :: GLfloat)] })
    
    translate' 0 (-1.0) 0
    scale' (1/(fromIntegral cubeSize))
    cubes <- readIORef cubesToRender
    mapM_ renderCube cubes
    
    flush
    swapBuffers
    endProf pb "Display"
    beginProf pb "Other"

initWnd :: IO ()
initWnd = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    
    createWindow "Shader Test"
    fullScreen
    cursor $= None
    
    depthFunc $= Just Less
    (Size sw sh) <- get screenSize
    viewport $= (Position 0 0, Size sw sh)
    pointerPosition $= Position (fromIntegral $ div sw 2) (fromIntegral $ div sh 2)
    
    matrixMode $= Projection
    loadIdentity
    let aspect = (fromIntegral sw)/(fromIntegral sh)
    perspective 30 aspect 0.1 100
    
    return ()

mouseMotion :: IORef State -> Position -> IO ()
mouseMotion state (Position x y) = do
    (Size sw sh) <- get screenSize
    let (scx, scy) = (fromIntegral $ div sw 2, fromIntegral $ div sh 2)
    pointerPosition $= Position (floor scx) (floor scy)
    
    let (mx, my) = (((fromIntegral x) - scx)/scx, (scy - (fromIntegral y))/scy)
    state $~ (\st -> st { stMouseX = mx, stMouseY = my })
    return ()

keyboard pb (Char 'q') Down _ _ = printProf pb >> exitSuccess
keyboard _ _ _ _ _ = return ()
