module TestZ where

import PeanoZ
import Stampa

testMonoide :: IO ()
testMonoide =
    if all (/= False) (map controllo [min..max])
    then corretto "Test monoide passato"
    else errato "Test monoide non passato"

    where min = fromInteger (-100) :: Intero
          max = fromInteger 100 :: Intero
          controllo = \x -> (x <> mempty == x) || (mempty <> x == x)