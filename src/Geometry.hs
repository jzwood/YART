module Geometry where

-- Math from:
-- https://slides.com/sergejkosov/ray-geometry-intersection-algorithms/fullscreen

newtype Point = Point (Double, Double, Double)
  deriving (Show, Eq)
newtype Vector = Vector (Double, Double, Double)
  deriving (Show, Eq)

data Ray = Ray Point Vector
  deriving (Show, Eq)
data Sphere = Sphere { sCenter :: Point, radius :: Double  }
  deriving (Show, Eq)
data Plane = Plane { pCenter :: Point, pPoint :: Point, pNormal :: Vector }
  deriving (Show, Eq)

minus :: Point -> Point -> Vector
minus (Point (px1, py1, pz1)) (Point (px2, py2, pz2)) = Vector (px1 - px2, py1 - py2, pz1 - pz2)

dot :: Vector -> Vector -> Double
dot (Vector (vx1, vy1, vz1)) (Vector (vx2, vy2, vz2)) = (vx1 * vx2) + (vy1 * vy2) + (vz1 * vz2)

cross :: Vector -> Vector -> Vector
cross (Vector (vx1, vy1, vz1)) (Vector (vx2, vy2, vz2)) = Vector (vy1 * vz2 - vz1 * vy2, vz1 * vx2 - vx1 * vz2, vx1 * vy2 - vy1 * vx2)

scale :: Double -> Vector -> Vector
scale f (Vector (vx1, vy1, vz1)) = Vector (f * vx1, f * vy1, f * vz1)

plus :: Point -> Vector -> Point
plus (Point (px, py, pz)) (Vector (vx, vy, vz)) = Point (px + vx, py + vy, pz + vz)

mag :: Vector -> Double
mag (Vector (vx, vy, vz)) = sqrt $ vx^2 + vy^2 + vz^2

negate :: Vector -> Vector
negate (Vector (vx, vy, vz)) = Vector (-vx, -vy, -vz)

normalize :: Vector -> Vector
normalize v = scale (1 / (mag v)) v

raySphereIntersection :: Ray -> Sphere -> Maybe Point
raySphereIntersection (Ray o d) (Sphere c r)
   | delta'2 < 0 = Nothing  -- no intersection
   | delta'2 == 0 = Just b -- 1 intersection
   | ml < r = Nothing  -- ray is inside sphere
   | otherwise = Just $ o `plus` (scale (tb - (sqrt delta'2))  nd) -- 2 intersections
  where
    nd = normalize d
    l = c `minus` o
    ml = mag l
    tb = nd `dot` l
    b = o `plus` (scale tb nd)
    delta'2 = r^2 - (ml)^2 + tb^2

rayPlaneIntersection :: Ray -> Plane -> Maybe Point
rayPlaneIntersection (Ray o d) (Plane a p n)
  | (tnum == 0) || (tden == 0) = Nothing
  | t < 0 = Nothing
  | otherwise = Just p
  where
    nn = normalize n
    nd = normalize d
    tnum = (a `minus` o) `dot` nn
    tden = nd `dot` nn
    t = tnum / tden
    p = o `plus` (scale t nd)

getNormal :: Point -> Sphere -> Vector
getNormal p (Sphere c _) = normalize $ p `minus` c
