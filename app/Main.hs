module Main where

import Codec.Picture
import Geometry
import RayTracer

--saveSceneImage :: LightSource -> Eye -> Sphere -> Window -> Image PixelRGB8

ls = Point (5, 40, -10)
eye = Point (5, 5, 0)
spheres = [ Sphere { sCenter = Point (3, 2, -20) , radius = 2.5 }
          , Sphere { sCenter = Point (9, 2, -20) , radius = 2.5 }
          ]
window = Window { wNorm = Ray (Point (5, 5, -5)) (Vector (0, 0, -1))
                , up = Vector (0, 1, 0)
                , width = 5, height = 5, pxWidth = 1000, pxHeight = 1000
                }

main :: IO ()
--main = imageCreator "./pics/test.png"
main = writePng "./pics/scene.png" $ saveSceneImage ls eye spheres window
