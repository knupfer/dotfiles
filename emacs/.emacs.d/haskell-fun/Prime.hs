module Prime (prime) where

import Math.NumberTheory.Primes

prime :: Int -> Integer
prime t = primes !! t

