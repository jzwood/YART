module Main where

import Codec.Picture
import Geometry
import RayTracer

--saveSceneImage :: LightSource -> Eye -> Sphere -> Window -> Image PixelRGB8

ls = Point (5, 40, -10)
eye = Point (5, 5, 5)
sphere = Sphere { sCenter = Point (5, 3, -20)
                , radius = 3
                }
window = Window { wNorm = Ray (Point (5, 5, -5)) (Vector (0, 0, -1))
                , up = Vector (0, 1, 0)
                , width = 10, height = 5, pxWidth = 2000, pxHeight = 1000
                }

main :: IO ()
--main = imageCreator "./pics/test.png"
main = writePng "./pics/floor.png" $ saveSceneImage ls eye sphere window
