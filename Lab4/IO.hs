module IO where

import Minesweeper

import Data.Char

printMinefield :: Minefield -> IO ()
printMinefield (Minefield ms) = mapM_ printRow ms

printRow :: [Tile] -> IO ()
printRow ms = putStrLn (map tileToChar ms)

tileToChar :: Tile -> Char
tileToChar Unknown = '#'
tileToChar Mine = 'm'
tileToChar Empty = '0'
tileToChar (Num 0) = '0'
tileToChar (Num i) = intToDigit i
tileToChar Flag = 'X'

play :: IO ()
play = do print exampleMinefield
          putStrLn "Enter a coordinate to select it"
          putStr "Enter row: "
          putStr "Enter col: "
          play
          


showTheDifference :: IO ()
showTheDifference = do putStrLn "Enter two numbers:"
                       x <- readLn
                       y <- readLn
                       putStr "The difference is: "
                       print (x-y)
