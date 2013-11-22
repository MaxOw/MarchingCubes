module Graphics.Utilities.ViewFrustumCulling (
    ViewFrustum, viewFrustum, pointInsideViewFrustum,
    viewFrustumAABB, 
    liftV, liftV2, minV, maxV
) where

import Control.Applicative
import Data.Maybe

import Data.Vect.Float as Vect
import Data.Vect.Float.Instances

import Graphics.Utilities.Camera

viewFrustumAABB cam fov ratio near far =
    calcAABB [ntl, ntr, nbl, nbr, ftl, ftr, fbl, fbr]
    where
        pos = camPos cam
        dir = camDir cam
        up = camUp cam
        
        z = normalize $ pos - dir
        x = normalize $ up &^ z
        y = z &^ x 
        
        nearH = tan (fov / 2) * near
        nearW = nearH * ratio
        
        farH = tan (fov / 2) * far
        farW = farH * ratio
        
        ntl = pos + dir&*near + y&*nearH - x&*nearW
        ntr = pos + dir&*near + y&*nearH + x&*nearW
        nbl = pos + dir&*near - y&*nearH - x&*nearW
        nbr = pos + dir&*near - y&*nearH + x&*nearW
          
        ftl = pos + dir&*far + y&*farH - x&*farW
        ftr = pos + dir&*far + y&*farH + x&*farW
        fbl = pos + dir&*far - y&*farH - x&*farW
        fbr = pos + dir&*far - y&*farH + x&*farW

liftV f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
liftV2 f (Vec3 x0 y0 z0) (Vec3 x1 y1 z1) = Vec3 (f x0 x1) (f y0 y1) (f z0 z1)
minV v0 v1 = liftV2 min v0 v1
maxV v0 v1 = liftV2 max v0 v1

calcAABB :: [Vec3] -> (Vec3, Vec3)
calcAABB [] = (zero, zero) -- error "calcAABB [] - Calculating bounding box with no points"
--calcAABB pl = foldl (\v (mmv, mxv) -> (min <$> v <*> mmv, max <$> v <*> mxv)) (h, h) pl
calcAABB pl = foldl (\(mmv, mxv) v -> (minV v mmv, maxV v mxv)) (h, h) pl
    where h = head pl

data Plane = Plane
   { pNorm  :: Vec3
   , pD     :: Float }

distFromPlane :: Plane -> Vec3 -> Float
distFromPlane (Plane n d) pt = n &. pt + d

data ViewFrustum = ViewFrustum
   { vfNearPlane      :: Plane
   , vfFarPlane       :: Plane
   , vfTopPlane       :: Plane
   , vfBottomPlane    :: Plane
   , vfLeftPlane      :: Plane
   , vfRightPlane     :: Plane }

viewFrustum cam fov ratio near far = ViewFrustum np fp tp bp lp rp
    where
        pos = camPos cam
        dir = camDir cam
        up = camUp cam
        
        z = normalize $ pos - dir
        x = normalize $ up &^ z
        y = z &^ x
        
        nc = pos - (z &* near)
        fc = pos - (z &* far)
        
        nh = tan (fov * (pi / 180) * 0.5) * near
        nw = nh * ratio
        
        plane n p = Plane n $ negate (n &. p)
        
        np = plane (neg z) nc
        fp = plane z fc
        tp = plane (x &^ normalize (dp - pos)) dp where dp = nc - (y &* nh)
        bp = plane (normalize (dp - pos) &^ x) dp where dp = nc + (y &* nh)
        lp = plane (y &^ normalize (dp - pos)) dp where dp = nc + (x &* nw)
        rp = plane (normalize (dp - pos) &^ y) dp where dp = nc - (x &* nw)

pointInsideViewFrustum :: ViewFrustum -> Vec3 -> Maybe Float
pointInsideViewFrustum vf pt =
    isOutsidePlane (vfNearPlane vf) pt >>=
    minMaybe vfFarPlane >>=
    minMaybe vfTopPlane >>=
    minMaybe vfBottomPlane >>=
    minMaybe vfLeftPlane >>=
    minMaybe vfRightPlane
    where 
        minMaybe f b = min <$> isOutsidePlane (f vf) pt <*> pure b
        isOutsidePlane pl pt = if d < 0 then Nothing else Just d
            where d = distFromPlane pl pt
