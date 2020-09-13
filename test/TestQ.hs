module TestQ (testQ) where

import Control.Monad (when)
import PeanoQ (Razionale)

testQ :: IO ()
testQ = do
    testMonoide

testMonoide :: IO ()
testMonoide = when (any (/= True) (controllo [min..max])) $ fail "monoide Q"
    where min = -100 :: Razionale
          max =  100 :: Razionale
          controllo = map (\x -> (x <> mempty == x) || (mempty <> x == x))