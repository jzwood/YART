{-# LANGUAGE NamedFieldPuns #-}

module RayTracer where

import Data.Functor
import Data.Function
import Data.Maybe
import Codec.Picture
import Geometry
import Utils

 -- Window: pixel screen through which rays are cast through from Eye
data Window = Window { wNorm :: Ray, up :: Vector, width :: Double, height :: Double, pxWidth :: Integer, pxHeight :: Integer }
data RGB = RGB { red :: Integer, green :: Integer, blue :: Integer }

type LightSource = Point
type Eye = Point

white = PixelRGB8 255 255 255
black = PixelRGB8 0 0 0

pixelToRay :: Integer -> Integer -> Eye -> Window -> Ray
pixelToRay x y eye (Window { wNorm = Ray origin vNorm, up, width, height, pxWidth, pxHeight }) = Ray point $ normalize (point `minus` eye)
   where
      left = up `cross` vNorm
      right = Geometry.negate left
      down = Geometry.negate up
      topLeft = origin `plus` (scale 0.5 left) `plus` (scale 0.5 up)
      xd = fromIntegral x
      yd = fromIntegral y
      pxWd = fromIntegral pxWidth
      pyHd = fromIntegral pxHeight
      point = topLeft `plus` (scale (xd * (width / pxWd)) right) `plus` (scale (yd * (height / pyHd)) down)

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

rgbToPixelRGB8 :: RGB -> PixelRGB8
rgbToPixelRGB8  (RGB red green blue) = PixelRGB8 (fromIntegral red) (fromIntegral green) (fromIntegral blue)


snap :: Integer -> Point -> Point
snap prec (Point (px, py, pz)) =
   let
      snap' = round' prec
   in
   Point (snap' px, snap' py, snap' pz)

--bounce :: Ray -> Maybe Point
--bounce (Ray p v) =

rayTrace :: LightSource -> Eye -> Sphere -> Window -> (Int -> Int -> PixelRGB8)
rayTrace ls eye sphere window =
   let
      floorColor :: LightSource -> Point -> RGB
      floorColor ls (Point (x, 0, z)) =
         if (odd (floor x) && odd (floor z)) || (even (floor x) && even (floor z))
         then RGB 0 0 0
         else RGB 255 255 255
      floorColor _ p = error $ "floor color somehow given a point that does not exist: " <> show p
      --floorColor _ = RGB 0 255 0
      pixelToColor :: Int -> Int -> PixelRGB8
      pixelToColor x y =
         pixelToRay (fromIntegral x) (fromIntegral y) eye window
         & (\ray -> rayPlaneIntersection ray plane)  -- & rayPlaneIntersection plane
         <&> snap 6
         <&> floorColor ls
         <&> rgbToPixelRGB8
         & fromMaybe black
   in
      pixelToColor
   where
      plane = Plane { pCenter = Point (0, 0, 0), pPoint = Point (1, 0, 0), pNormal = Vector (0, 1, 0) }

{-
if (first ray hits sphere) {
   find reflected ray goes to space -> determine color based off surface color and angle between ray and ray to light source
   if (ray has a clear view of light source) {
      determine color based off surface color and angle between ray and ray to light source
   }
   else {
   }
}
-}

saveSceneImage :: LightSource -> Eye -> Sphere -> Window -> Image PixelRGB8
saveSceneImage ls eye sphere window@(Window { pxWidth, pxHeight }) = generateImage (rayTrace ls eye sphere window) (fromIntegral pxWidth) (fromIntegral pxHeight)
