module Utils where

round' :: Integer -> Double -> Double
round' n x = (fromIntegral . round $ x * t) / t
    where t = 10^n

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs
