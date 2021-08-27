module Main where

import Geometry
import RayTracer

--saveSceneImage :: LightSource -> Eye -> Sphere -> Window -> Image PixelRGB8

ls = Point (5, 40, -10)
eye = Point (5, 5, 0)
--spheres = [ Sphere { sCenter = Point (3, 2, -20) , radius = 2.5 }
          --, Sphere { sCenter = Point (9, 2, -20) , radius = 2.5 }
          --]
--window = Window { wNorm = Ray (Point (5.2, 4.5, -5)) (Vector (0, 0, -1))
                --, up = Vector (0, 1, 0)
                --, width = 4, height = 2.5, pxWidth = 800, pxHeight = 500
                --}

spheres = [ Sphere { sCenter = Point (5, 5, -20) , radius = 5 }
          , Sphere { sCenter = Point (6, 5, -5) , radius = 5 }
          , Sphere { sCenter = Point (13, 2, -19) , radius = 2 }
          ]
window = Window { wNorm = Ray (Point (5.2, 4.5, -5)) (Vector (0, 0, -1))
                , up = Vector (0, 1, 0)
                , width = 4, height = 2.5, pxWidth = 4000, pxHeight = 2500
                }

main :: IO ()
main = writeFile "./pics/scene.ppm" $ saveSceneImage ls eye spheres window
