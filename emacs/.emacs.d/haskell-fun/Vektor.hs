module Vektor (Vektor.length) where

length :: [Double] -> Double
length xs = sum $ map (^(2 :: Int)) xs


