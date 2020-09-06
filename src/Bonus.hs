module Bonus where

import Data.Numbers.Primes (isPrime)
import Flow ((|>))

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
    | 0 < n && n <= k  = n
    | otherwise        = n * (multifattoriale k (n - k))

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

sudan :: (Num a, Eq a) => a -> a -> a -> a
sudan n x y =
    case (n, y) of
        (0, _) -> x + y
        (_, 0) -> x
        (_, _) -> sudan (n - 1) (sudan n x (y - 1)) ((sudan n x (y - 1)) + y)

ackermann :: (Num a, Eq a) => a -> a -> a -> a
ackermann m n p =
    case (n, p) of
        (_, 0) -> m + n
        (0, 1) -> 0
        (0, _) -> m
        (_, _) -> ackermann m (ackermann m (n - 1) p) (p - 1)

ackermannPéter :: (Num a, Eq a) => a -> a -> a
ackermannPéter m n =
    case (m, n) of
        (0, _) -> n + 1
        (_, 0) -> ackermannPéter (m - 1) 1
        (_, _) -> ackermannPéter (m - 1) (ackermannPéter m (n - 1))


---- Funzioni personali

multisuperfattoriale :: (Ord a, Num a, Enum a) => a -> a -> a
multisuperfattoriale k n
    | -k < n && n <= 0 = 1
    | 0 < n && n <= k  = n
    | otherwise        = (fattoriale n) * (multifattoriale k (n - k))

fiboriale :: (Ord a, Num a) => a -> a
fiboriale n = takeWhile (<= n) fib |> product
    where fib = 1 : 1 : zipWith (+) fib (tail fib)

superfiboriale :: (Num a, Ord a) => a -> a
superfiboriale n = map fiboriale fibo |> product
    where
        fib = 1 : 1 : zipWith (+) fib (tail fib)
        fibo = takeWhile (<= n) fib

iperfiboriale :: Num a => Int -> a
iperfiboriale n = take n fib |> product
    where fib = 1 : 1 : zipWith (+) fib (tail fib)