module Utils where

round' :: Integer -> Double -> Double
round' n x = (fromIntegral . round $ x * t) / t
    where t = 10^n
