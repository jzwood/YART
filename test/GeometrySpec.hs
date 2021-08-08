module GeometrySpec (spec) where

import Test.Hspec
import Geometry

spec :: Spec
spec = do
  describe "geometry math" $ do
    it "minus" $ do
      (Point (1, 0, 0)) `minus` (Point (0, 0, 0)) `shouldBe` (Vector (1, 0, 0))
      (Point (4, 1, 0)) `minus` (Point (3, 0, 1)) `shouldBe` (Vector (1, 1, -1))

    it "dot" $ do
      (Vector (0, 1, 0)) `dot` (Vector (0, 1, 0)) `shouldBe` 1
      (Vector (0, 1, 0)) `dot` (Vector (0, 0, 1)) `shouldBe` 0

    it "scale" $ do
      scale 3 (Vector (0, 1, 0)) `shouldBe` Vector (0, 3, 0)

    it "plus" $ do
      (Point (0, 0, 1)) `plus` (Vector (1, 1, 1)) `shouldBe` Point (1, 1, 2)

    it "mag" $ do
      mag (Vector (1, 1, 1)) `shouldBe` sqrt 3

    it "normalize" $ do
      mag (normalize (Vector (5, 5, 5))) `shouldBe` 1

  describe "ray sphere intersection" $ do
    it "intersection 1" $
      let
        camera = Ray (Point (0,0,0)) (Vector (1,0,0))
        sphere = Sphere { sCenter = Point (4,0,0), radius = 1 }
      in
        raySphereIntersection camera sphere `shouldBe` (Just $ Point (3, 0, 0))

    it "intersection 2" $
      let
        camera = Ray (Point (0,0,1)) (Vector (0,0,1))
        sphere = Sphere { sCenter = Point (0,0,10), radius = 2.5 }
      in
        raySphereIntersection camera sphere `shouldBe` (Just $ Point (0, 0, 7.5))

    it "intersection unnormalized ray" $
      let
        camera = Ray (Point (0,0,1)) (Vector (0,0,8))
        sphere = Sphere { sCenter = Point (0,0,10), radius = 1 }
      in
        raySphereIntersection camera sphere `shouldBe` (Just $ Point (0, 0, 9))

    it "intersection ray from surface" $
      let
        camera = Ray (Point (0,0,0)) (Vector (0,1,0))
        sphere = Sphere { sCenter = Point (0,1,0), radius = 1 }
      in
        raySphereIntersection camera sphere `shouldBe` (Just $ Point (0, 0, 0))

    it "intersection ray inside surface" $
      let
        camera = Ray (Point (0,0,0)) (Vector (0,1,0))
        sphere = Sphere { sCenter = Point (0,0,0), radius = 2 }
      in
        raySphereIntersection camera sphere `shouldBe` Nothing

    it "no intersection" $
      let
        camera = Ray (Point (0,0,0)) (Vector (0,1,0))
        sphere = Sphere { sCenter = Point (0,0,8), radius = 2 }
      in
        raySphereIntersection camera sphere `shouldBe` Nothing

  describe "ray plane intersection" $ do
    it "intersection 1" $
      let
        camera = Ray (Point (0,4,0)) (Vector (0,-4,0))
        plane = Plane { pCenter = Point (0,0,0), pPoint = Point (0, 0, 1), pNormal = Vector (0, 1, 0) }
      in
        rayPlaneIntersection camera plane `shouldBe` (Just $ Point (0, 0, 0))

    it "intersection 2" $
      let
        camera = Ray (Point (1,1,1)) (Vector (-1,-1,-1))
        plane = Plane { pCenter = Point (0,0,0), pPoint = Point (0, 1, 1), pNormal = Vector (1, 1, 1) }
      in
        rayPlaneIntersection camera plane `shouldBe` (Just $ Point (0, 0, 0))

    it "no intersection 1" $
      let
        camera = Ray (Point (0,3,0)) (Vector (0,1,0))
        plane = Plane { pCenter = Point (0,0,0), pPoint = Point (1, 0, 0), pNormal = Vector (0, 1, 0) }
      in
        rayPlaneIntersection camera plane `shouldBe` Nothing

    it "no intersection (parellel)" $
      let
        camera = Ray (Point (2,3,0)) (Vector (2,0,0))
        plane = Plane { pCenter = Point (0,0,0), pPoint = Point (1, 0, 0), pNormal = Vector (0, 1, 0) }
      in
        rayPlaneIntersection camera plane `shouldBe` Nothing

    it "infinite intersections" $
      let
        camera = Ray (Point (0,0,0)) (Vector (7,0,0))
        plane = Plane { pCenter = Point (0,0,0), pPoint = Point (1, 0, 0), pNormal = Vector (0, 1, 0) }
      in
        rayPlaneIntersection camera plane `shouldBe` Nothing

