module Graphics.Utilities.Profiling (
    ProfileBase, profileBase, beginProf, endProf, printProf
) where

import Control.Monad
import Data.Function
import Data.IORef
import Data.Maybe
import Data.List
import Graphics.UI.GLUT
import System.CPUTime
import Text.Printf

data Profile = Profile
   { prNum  :: Double
   , prMin  :: Double
   , prMax  :: Double
   , prSum  :: Double }
   deriving (Show)

newProfile :: Double -> Profile
newProfile t = Profile
   { prNum  = 0
   , prMin  = t
   , prMax  = t
   , prSum  = 0 }

profile :: Profile -> Double -> Profile
profile (Profile n mn mx sm) dt = (Profile (n+1) (min dt mn) (max dt mx) (sm + dt))

data ProfileBase = ProfileBase
   { pbProfiles :: [(String, Profile)]
   , pbOpen     :: [(String, Double)] }
   deriving (Show)

profileBase :: IO (IORef ProfileBase)
profileBase = newIORef $ ProfileBase [] []

updateRecord :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
updateRecord kv = (:) kv . deleteBy ((==) `on` fst) kv

modifyIO :: IORef a -> (a -> IO a) -> IO ()
modifyIO r f = readIORef r >>= f >>= writeIORef r

getTime = liftM (fromIntegral) (get elapsedTime)
--getTime = liftM ((/(1000**3)) . fromIntegral) getCPUTime

beginProf :: IORef ProfileBase -> String -> IO ()
beginProf pb pn = do
    et <- getTime
    modifyIORef pb $ beginProfP pn et

endProf :: IORef ProfileBase -> String -> IO ()
endProf pb pn = do
    et <- getTime
    modifyIORef pb $ endProfP pn et

beginProfP :: String -> Double -> ProfileBase -> ProfileBase
beginProfP pn et pb@(ProfileBase { pbOpen = pbo }) =
    (pb { pbOpen = updateRecord (pn, et) pbo })

endProfP :: String -> Double -> ProfileBase -> ProfileBase
endProfP pn et pb@(ProfileBase pbp pbo) =
    if (isNothing ot) then pb -- Lacking beginProf: Exception ?
    else (ProfileBase ((pn, profile pr dt):npbp) npbo)
    where
        npbo = deleteBy ((==) `on` fst) (pn, undefined) pbo
        npbp = deleteBy ((==) `on` fst) (pn, undefined) pbp
        pr = fromMaybe (newProfile dt) $ lookup pn pbp
        dt = et - (fromJust ot)
        ot = lookup pn pbo

printProf :: IORef ProfileBase -> IO ()
printProf r = do
    pb <- readIORef r
    printf "%-30s%-12s%-12s%-12s%-12s\n" "Name" "Num" "Min(ms)" "Max(ms)" "Avr(ms)"
    forM_ (pbProfiles pb) $ \(n, Profile pn pmn pmx ps) -> do
        printf "%-30s%-12.0f%-12.3f%-12.3f%-10.3f\n" n pn pmn pmx (ps/pn)

