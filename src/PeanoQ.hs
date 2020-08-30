{-# LANGUAGE LambdaCase #-}

module PeanoQ where

import GHC.Real
import Flow
import PeanoZ hiding (normalizza)

data Razionale = Intero :/ Intero
    deriving (Show, Read, Eq)

normalizza :: Razionale -> Razionale
normalizza (x :/ y)
    | y < 0 = (negate x) :/ (negate y) |> normalizza
    | otherwise = x' :/ y'
        where
            mcm = gcd x y |> toInteger .> fromInteger
            x' = x |> toInteger .> fromInteger |> flip (/) mcm |> truncate .> fromInteger
            y' = y |> toInteger .> fromInteger |> flip (/) mcm |> truncate .> fromInteger


instance Ord Razionale where
    compare (x :/ y) (x' :/ y') = compare (x * y') (x' * y)

instance Num Razionale where
    (+) (x :/ y) (x' :/ y')
        | y == y'   = (x + x') :/ y |> normalizza
        | otherwise = ((x * y') + (x' * y)) :/ (lcm y y') |> normalizza

    (*) (x :/ y) (x' :/ y') = (x * x') :/ (y * y') |> normalizza

    abs (x :/ y)
        | x < 0     = (negate x) :/ y
        | otherwise = x :/ y

    signum (x :/ y)
        | x == 0 = 0
        | x < 0 = -1
        | otherwise = 1

    fromInteger x = (fromInteger x) :/ 1

    negate (x :/ y) = (negate x) :/ y |> normalizza

--instance Enum Razionale where

instance Real Razionale where
    toRational (x :/ y) = (toInteger x) :% (toInteger y)

--instance Integral Razionale where

instance Fractional Razionale where
    fromRational (x :% y) = (fromInteger x) :/ (fromInteger y)

    recip (x :/ y) = y :/ x

instance Semigroup Razionale where
    (<>) = (+)

instance Monoid Razionale where
    mempty = 0 :/ 1