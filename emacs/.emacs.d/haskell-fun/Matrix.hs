module Matrix (identity,dyadic,transpose,isIdentity) where

import qualified Data.List as L

isIdentity :: [[Int]] -> Bool
isIdentity xs = xs == identity (length xs)

transpose :: [[Int]] -> [[Int]]
transpose = L.transpose

identity :: Int -> [[Int]]
identity n
  | n > 1 = L.nub $ L.permutations $ 1 : replicate (n-1) 0
  | otherwise = [[1]]

dyadic :: ([Int],[Int]) -> [[Int]]
dyadic (xs,ys) = map (\x -> map (x*) ys) xs
