module Geometry where


newtype Point = Point (Double, Double, Double)
  deriving (Show, Eq)
newtype Vector = Vector (Double, Double, Double)
  deriving (Show, Eq)
newtype Plane = Plane (Point, Point, Point)
  deriving (Show, Eq)

data Ray = Ray Point Vector
  deriving (Show, Eq)
data Sphere = Sphere { sCenter :: Point, radius :: Double  }
  deriving (Show, Eq)

minus :: Point -> Point -> Vector
minus (Point (px1, py1, pz1)) (Point (px2, py2, pz2)) = Vector (px2 - px1, py2 - py1, pz2 - pz1)

dot :: Vector -> Vector -> Double
dot (Vector (vx1, vy1, vz1)) (Vector (vx2, vy2, vz2)) = (vx1 * vx2) + (vy1 * vy2) + (vz1 * vz2)

scale :: Double -> Vector -> Vector
scale f (Vector (vx1, vy1, vz1)) = Vector (f * vx1, f * vy1, f * vz1)

plus :: Point -> Vector -> Point
plus (Point (px, py, pz)) (Vector (vx, vy, vz)) = Point (px + vx, py + vy, pz + vz)

mag :: Vector -> Double
mag (Vector (vx, vy, vz)) = sqrt $ vx^2 + vy^2 + vz^2

normalize :: Vector -> Vector
normalize v = scale (1 / (mag v)) v

findClosestIntersection :: Ray -> Sphere -> Maybe Point
findClosestIntersection (Ray o d) (Sphere c r)
   | delta'2 < 0 = Nothing
   | delta'2 == 0 = Just b
   | otherwise = Just $ o `plus` (scale (tb - (sqrt delta'2))  nd)
  where
    nd = normalize d
    l = c `minus` o
    tb = d `dot` l
    b = o `plus` (scale tb nd)
    delta'2 = r^2 - (mag l)^2 + tb^2
