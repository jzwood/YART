module PPM where

import Data.List

data RGB = RGB { red :: Integer, green :: Integer, blue :: Integer }
  deriving (Eq)

instance Show RGB where
  show (RGB r g b) = intercalate " " $ map show [r, g, b]

genPPM :: (Integer -> Integer -> RGB) -> Integer -> Integer -> String
genPPM f w h = (intercalate " " $ header ++ pixels) ++ "\n"
  where
    header = "P3" : (map show $ [w, h, 255])
    pixels = "\n" : [show (f x y) | y <- [0..(h - 1)], x <- [0..(w - 1)]]
