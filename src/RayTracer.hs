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
  deriving (Show, Eq)
data RGB = RGB { red :: Integer, green :: Integer, blue :: Integer }
  deriving (Show, Eq)

type LightSource = Point
type Eye = Point

white = PixelRGB8 255 255 255
black = PixelRGB8 0 0 0

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

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

rgbToPixelRGB8 :: RGB -> PixelRGB8
rgbToPixelRGB8  (RGB red green blue) = PixelRGB8 (fromIntegral red) (fromIntegral green) (fromIntegral blue)

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

--bounce :: Ray -> Maybe Point
--bounce (Ray p v) =

rayTrace :: LightSource -> Eye -> Sphere -> Window -> (Int -> Int -> PixelRGB8)
rayTrace ls eye sphere window =
   let
      floorColor :: Point -> RGB
      floorColor intersection@(Point (x, 0, z)) = scaleRGB brightness origColor
         where
            origColor =
               if (odd (floor x) && odd (floor z)) || (even (floor x) && even (floor z))
               then RGB 0 0 0
               else RGB 255 255 255
            theta = angle (eye `minus` intersection) (ls `minus` intersection)
            brightness = (sin theta) ^ 2
      floorColor p = error $ "floor color somehow given a point that does not exist: " <> show p
      --floorColor _ = RGB 0 255 0
      pixelToColor :: Int -> Int -> PixelRGB8
      pixelToColor x y =
         pixelToRay (fromIntegral x) (fromIntegral y) eye window
         & (\ray -> rayPlaneIntersection ray plane)  -- & rayPlaneIntersection plane
         <&> snap 6
         <&> floorColor
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
