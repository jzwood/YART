module Main where

import Codec.Picture
import Geometry
import RayTracer

--saveSceneImage :: LightSource -> Eye -> Sphere -> Window -> Image PixelRGB8

main :: IO ()
--main = imageCreator "./pics/test.png"
main = writePng "./pics/floor.png" $ saveSceneImage (Point (10, 30, 10)) (Point (5, 5, -5)) (Sphere { sCenter = Point (5, 3, 8), radius = 3}) (Window {wNorm = Ray (Point (5, 5, 5)) (Vector (0, 0, 1)), up = Vector (0, 1, 0), width = 10, height = 4,  pxWidth = 2000, pxHeight = 800})
