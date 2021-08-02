module RayTracer where

import Codec.Picture


newtype Point = Point (Integer, Integer, Integer)
newtype Vector = Vector (Double, Double, Double)
newtype Plane = Plane (Point, Point, Point)

data Sphere = Circle { sCenter :: Point, radius :: Double  }
 -- Window: pixel screen through which rays are cast through from Eye
data Window = Window { wCenter :: Point, up :: Vector, width :: Double, height :: Double, pxWidth :: Integer, pxHeight :: Integer }
data RGB = RGB { red :: Int, green :: Int, blue :: Int }

type LightSource = Point
type Eye = Point

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

rayTrace :: LightSource -> Eye -> Plane -> Sphere -> Window -> (Int -> Int -> PixelRGB8)
rayTrace = undefined

saveSceneImage :: LightSource -> Eye -> Plane -> Sphere -> Window -> Image PixelRGB8
saveSceneImage ls@(Point (lsX, lsY, lsZ)) eye@(Point (eX, eY, eZ)) plane sphere window@(Window { pxWidth = width, pxHeight = height }) = generateImage (rayTrace ls eye plane sphere window) (fromIntegral width) (fromIntegral height)
