module Bonus where

import Data.Numbers.Primes
import Flow
import PeanoZ

iperoperazione :: (Num a, Eq a) => a -> a -> a -> a
iperoperazione n a b =
    case (n, b) of
        (0, _) -> b + 1
        (1, 0) -> a
        (2, 0) -> 0
        (_, 0) -> 1
        (_, _) -> iperoperazione (n - 1) a (iperoperazione n a (b - 1))

fattoriale :: (Num a, Enum a) => a -> a
fattoriale n = product [1..n]

semifattoriale :: (Ord a, Num a) => a -> a
semifattoriale = multifattoriale 2

multifattoriale :: (Ord a, Num a) => a -> a -> a
multifattoriale k n
    | -k < n && n <= 0 = 1
    | 0 < n && n <= k = n
    | otherwise = n * (multifattoriale k (n - k))

primoriale :: Integral a => a -> a
primoriale n = filter isPrime [1..n] |> product

superfattoriale :: (Num a, Enum a) => a -> a
superfattoriale n = map fattoriale [1..n] |> product

superfattorialePickover :: (Num a, Eq a, Enum a) => a -> a
superfattorialePickover n = iperoperazione 4 (fattoriale n) (fattoriale n)

iperfattoriale :: (Num a, Eq a, Enum a) => a -> a
iperfattoriale n = map (\x -> iperoperazione 3 x x) [1..n] |> product

fattorialeAlternante :: (Num a, Enum a, Eq a) => a -> a
fattorialeAlternante n
    | n == 1 = 1
    | otherwise = fattoriale n - fattorialeAlternante (n - 1)