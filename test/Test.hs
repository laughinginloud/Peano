import TestZ (testZ)
import TestQ (testQ)

main :: IO ()
main = do
    testZ
    testQ
    putStrLn "Tutti i test superati"