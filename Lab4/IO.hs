module IO where

import Minesweeper
import Utils

import Data.Char

printMinefield :: Minefield -> IO ()
printMinefield (Minefield ms) = mapM_ printRow ms

printRow :: [Tile] -> IO ()
printRow ms = putStrLn (map tileToChar (ms))

tileToChar :: Tile -> Char
tileToChar Unknown = '#'
tileToChar Mine = 'm'
tileToChar Empty = '0'
tileToChar (Num i) = intToDigit i
tileToChar Flag = 'X'