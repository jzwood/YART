{-# LANGUAGE NamedFieldPuns #-}

module RayTracer where

import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Functor
import Data.Function
import Geometry
import PPM
import Utils

 -- Window: pixel screen through which rays are cast through from Eye
data Window = Window { wNorm :: Ray, up :: Vector, width :: Double, height :: Double, pxWidth :: Integer, pxHeight :: Integer }
  deriving (Show, Eq)

type LightSource = Point
type Eye = Point

white = RGB 255 255 255
black = RGB 0 0 0
red' = RGB 255 0 0
green' = RGB 0 255 0
blue' = RGB 0 0 255
gray = RGB 50 50 50

pixelToOrigin :: Window -> Point
pixelToOrigin (Window { wNorm = Ray origin vNorm, up, width, height }) = topLeft
   where
      left = normalize $ up `cross` vNorm
      right = Geometry.negate left
      nUp = normalize up
      down = Geometry.negate nUp
      topLeft = origin `plus` ((0.5 * width) `scale` left) `plus` ((0.5 * height) `scale` nUp)

pixelToRay :: Integer -> Integer -> Eye -> Window -> Ray
pixelToRay x y eye w@(Window { wNorm = Ray origin vNorm, up, width, height, pxWidth, pxHeight }) = Ray point $ normalize (point `minus` eye)
   where
      topLeft = pixelToOrigin w
      right = Geometry.negate $ normalize $ up `cross` vNorm
      down = Geometry.negate $ normalize up
      xd = fromIntegral x
      yd = fromIntegral y
      pxWd = fromIntegral pxWidth
      pyHd = fromIntegral pxHeight
      point = topLeft `plus` ((xd / (pxWd - 1) * width) `scale` right) `plus` ((yd / (pyHd - 1) * height) `scale` down)

scaleRGB :: Double -> RGB -> RGB
scaleRGB per (RGB red green blue) =
   let
      scale' :: Integer -> Integer
      scale' color = round (per * fromIntegral color)
   in
      RGB (scale' red) (scale' green) (scale' blue)

snap :: Integer -> Point -> Point
snap prec (Point (px, py, pz)) =
   let
      snap' = round' prec
   in
   Point (snap' px, snap' py, snap' pz)

bounce :: [Sphere] -> Plane -> Ray -> Maybe (Point, Ray)
bounce spheres plane ray@(Ray p v) =
   case (mRsiS, mPsi) of
     (Just (rsi, sphere), _) -> (\reflectedVector -> if Geometry.negate v == reflectedVector then Just (rsi, intoBlackHole) else Just (rsi, Ray rsi reflectedVector)) (reflect v (getSphereNormal rsi sphere))
     (_, Just psi) -> Just (psi, intoBlackHole) -- a ray that intersects with nothing
     _ -> Nothing
   where
      intoBlackHole = Ray (Point (0, -1, 0)) (Vector (0, -1, 0)) -- a ray that intersects with nothing
      mRsiS = spheres
             & mapMaybe (raySphereIntersection ray)
             & sortOn (\(i, s) -> mag $ p `minus` i)
             & maybeHead
      mPsi = rayPlaneIntersection ray plane

trackRay :: [Sphere] -> Plane -> Ray -> Maybe Point
trackRay spheres plane ray =
   let
      bounce' = bounce spheres plane
    in
      unfoldr bounce' ray
      & maybeLast

canLightSourceReachPoint :: LightSource -> Point -> Sphere -> Bool
canLightSourceReachPoint ls point sphere = raySphereIntersection ray sphere & isNothing
   where
      ray = Ray ls (point `minus` ls)

calculateColor :: LightSource -> Eye -> [Sphere] -> Point -> RGB
calculateColor ls eye spheres intersection@(Point (x, y, z)) = scaleRGB (visibility * brightness) floorColor
   where
      floorColor =
         if (odd (floor x) && odd (floor z)) || (even (floor x) && even (floor z))
            then RGB 0 0 0
      else RGB 255 255 255
      theta = angle (eye `minus` intersection) (ls `minus` intersection)
      brightness = (sin theta) ^ 2
      --distance = logistic 0.2 $ mag $ ls `minus` intersection
      --distance = minimum [1, 30 / (mag $ ls `minus` intersection)]
      visibility = if all (canLightSourceReachPoint ls intersection) spheres then 1 else 0.2
--calculateColor ls p = RGB gray gray gray

rayTrace :: LightSource -> Eye -> [Sphere] -> Window -> (Integer -> Integer -> RGB)
rayTrace ls eye spheres window =
   let
      pixelToColor :: Integer -> Integer -> RGB
      pixelToColor x y =
         pixelToRay (fromIntegral x) (fromIntegral y) eye window
         & trackRay spheres plane
         <&> snap 6
         <&> calculateColor ls eye spheres
         & fromMaybe black
   in
      pixelToColor
   where
      plane = Plane { pCenter = Point (0, 0, 0), pNormal = Vector (0, 1, 0) }

saveSceneImage :: LightSource -> Eye -> [Sphere] -> Window -> String
saveSceneImage ls eye spheres window@(Window { pxWidth, pxHeight }) = genPPM (rayTrace ls eye spheres window) pxWidth pxHeight
