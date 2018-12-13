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
tileToChar Empty = '■'
tileToChar (Num 0) = '0'
tileToChar (Num i) = intToDigit i
tileToChar Flag = 'X'

examplePlay :: IO ()
examplePlay = play allUnknowns exampleMinefield

play :: Minefield -> Minefield -> IO ()
play userMinefield minefield = 
   do printMinefield userMinefield
      putStrLn "Enter a command ('f' to set flag, 's' to select or 'q' to quit"
      char <- getChar
      if char == 'q' then print "You suck"
      else if char == 'f' then
         do p <- askForPos
            play (update userMinefield p Flag) minefield

      else if char == 's' then 
         do p <- askForPos
            let userM = revealTile userMinefield minefield p
            if checkIfWon userMinefield minefield then print "Congrats, you won"
            else play userM minefield
      else 
         do putStr "Invalid input"
            play userMinefield minefield

checkIfWon :: Minefield -> Minefield-> Bool
checkIfWon userMinefield minefield = 
   (amountTile userMinefield Unknown) + (amountTile userMinefield Flag) == amountTile minefield Mine

askForPos :: IO (Pos)
askForPos = 
   do putStr "\nEnter row: "
      r <- readLn
      putStr "Enter col: "
      c <- readLn
      return (r,c)