module Stampa (corretto, errato, fine) where

import System.Console.ANSI

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

fine = colorStrLn Vivid Yellow "\nFine dei test"