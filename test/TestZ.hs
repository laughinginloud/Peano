module TestZ (testZ) where

import Control.Monad (when)
import PeanoZ (Intero)

testZ :: IO ()
testZ = do
    testMonoide

testMonoide :: IO ()
testMonoide = when (any (/= True) (controllo [min..max])) $ fail "monoide Z"
    where min = -100 :: Intero
          max =  100 :: Intero
          controllo = map (\x -> (x <> mempty == x) || (mempty <> x == x))