{-# LANGUAGE LambdaCase #-}

module PeanoQ where

import GHC.Real (Ratio((:%)))
import Flow ((.>), (|>))
import PeanoZ (Intero)

data Razionale = Intero :/ Intero
    deriving (Show, Read, Eq)

normalizza :: Razionale -> Razionale
normalizza (x :/ y)
    | y < 0     = (negate x) :/ (negate y) |> normalizza
    | otherwise = x' :/ y'
        where
            mcd = gcd x y |> toInteger
            x'  = x |> toInteger |> flip quot mcd |> fromInteger
            y'  = y |> toInteger |> flip quot mcd |> fromInteger

instance Ord Razionale where
    compare (x :/ y) (x' :/ y') = compare (x * y') (x' * y)

instance Num Razionale where
    (+) (x :/ y) (x' :/ y')
        | y == y'   = (x + x') :/ y
        | otherwise = ((x * y') + (x' * y)) :/ (lcm y y')
        |> normalizza

    (*) (x :/ y) (x' :/ y') = (x * x') :/ (y * y') |> normalizza

    abs = normalizza .> abs'
        where
            abs' (x :/ y)
                | x < 0     = (negate x) :/ y
                | otherwise = x :/ y

    signum = normalizza .> signum'
        where
            signum' (x :/ y)
                | x == 0    = 0
                | x < 0     = -1
                | otherwise = 1

    fromInteger x = (fromInteger x) :/ 1

    negate (x :/ y) = (negate x) :/ y |> normalizza

instance Enum Razionale where
    toEnum = toInteger .> fromInteger

    fromEnum (x :/ y) = quot x y |> toInt
        where toInt = toInteger .> fromInteger

instance Real Razionale where
    toRational (x :/ y) = (toInteger x) :% (toInteger y)

instance Fractional Razionale where
    fromRational (x :% y) = (fromInteger x) :/ (fromInteger y)

    recip (x :/ y) = y :/ x |> normalizza

instance Semigroup Razionale where
    (<>) = (+)

instance Monoid Razionale where
    mempty = 0 :/ 1