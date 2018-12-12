module IO where

import Minesweeper
import Utils

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

play :: Minefield -> IO ()
play m = do print m
            putStrLn "Enter a command ('f' to set flag, 's' to select or 'q' to quit"
            char <- getChar
            if char == 'q' then print "You suck"
            else if char == 'f' then
              do putStr "Enter row: "
                 r <- readLn
                 putStr "Enter col: "
                 c <- readLn
                 play (update m (r,c) Flag)
                 
            else if char == 's' then 
              do putStr "Enter row: "
                 r <- readLn
                 putStr "Enter col: "
                 c <- readLn
                 play (update m (r,c) Mine)
            else do print "Invalid input"
                    play m


showTheDifference :: IO ()
showTheDifference = do putStrLn "Enter two numbers:"
                       x <- readLn
                       y <- readLn
                       putStr "The difference is: "
                       print (update exampleMinefield (x,y) Flag)
