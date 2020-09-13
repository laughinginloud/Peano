module TestZ (testZ) where

import PeanoZ (Intero)
import Control.Monad (when)

testZ :: IO ()
testZ = do
    testMonoide

testMonoide :: IO ()
testMonoide = when (any (/= True) (map controllo [min..max])) $ fail "monoide"
    where min = fromInteger (-100) :: Intero
          max = fromInteger 100 :: Intero
          controllo = \x -> (x <> mempty == x) || (mempty <> x == x)