import PeanoZ
import System.Console.ANSI

main :: IO ()
main =
    testMonoide
    >> colorStrLn Vivid Yellow "\nFine dei test"

colorStrLn :: ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg str = do
    setSGR [SetColor Foreground fgi fg]
    putStr str
    setSGR []
    putStrLn ""

corretto :: String -> IO ()
corretto = colorStrLn Vivid Green

errato :: String -> IO ()
errato = colorStrLn Vivid Red

---- TEST

testMonoide :: IO ()
testMonoide =
    case filter (\x -> (x <> mempty /= x) || (mempty <> x /= x)) [min..max] of
        [] -> corretto "Test monoide passato"
        _  -> errato "Test monoide non passato"
    where min = fromInteger (-100) :: Intero
          max = fromInteger 100 :: Intero