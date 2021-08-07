module GeometrySpec (spec) where

import Test.Hspec
import Geometry

spec :: Spec
spec = do
  describe "ray sphere intersection" $ do
    it "works 1" $
      let
        camera = Ray (Point (0,0,0)) (Vector (1,0,0))
        sphere = Sphere { sCenter = Point (4,0,0), radius = 1 }
      in
        (findClosestIntersection camera sphere) `shouldBe` (Just $ Point (3, 0, 0))

    it "works 2" $
      2 `shouldBe` 4
