module IO where

import Minesweeper
import Utils

import Data.Char

printMinefield :: Minefield -> IO ()
printMinefield (Minefield ms) = do putStr "\n    0 "
                                   putStrLn $ concat $ map showIndexCol [1..width-1]
                                   mapM_ printRow (zip ms ([0..height-1]))
                              where width = length (head ms)
                                    height = length ms

showIndexCol :: Int -> String
showIndexCol i | i < 10 = ' ' : show i ++ " "
               | otherwise = show i ++ " "

printRow :: ([Tile],Int) -> IO ()
printRow (ms,r) = putStrLn ((showIndexCol r) ++ concat (map tileToChar ms))

tileToChar :: Tile -> String
tileToChar Unknown   = "[#]"
tileToChar Mine      = "[m]"
tileToChar Empty     = " ■ "
tileToChar (Num 0)   = "[0]"
tileToChar (Num i)   = " " ++ intToDigit i : " "
tileToChar Flag      = "[X]"

examplePlay :: IO ()
examplePlay = play exampleGame

exampleGame :: Game
exampleGame = Game {userM = allUnknowns, logicM = exampleMinefield, hasWon = False, hasLost = False}

play :: Game -> IO ()
play Game {userM = userMinefield, logicM = minefield, hasWon = won, hasLost = lost} = 
   do printMinefield userMinefield
      putStrLn "Enter a command ('f' to set flag, 's' to select or 'q' to quit"
      char <- getChar
      if char == 'q' then print "Bye Bye"
      else if char == 'f' then
         do p <- askForPos
            play Game {userM = (update userMinefield p Flag), logicM = minefield, hasWon = won, hasLost = lost}
      else if char == 's' then 
         do p <- askForPos
            if (getTileAtPos userMinefield p) == Flag then
               do c <- askForConfirmation
                  if 'y' /= c then play Game {userM = userMinefield, logicM = minefield, hasWon = won, hasLost = lost}
                  else 
                     do let um = revealTile userMinefield minefield p
                        if checkIfWon userMinefield minefield then print "Congrats, you won"
                        else play Game {userM = um, logicM = minefield, hasWon = won, hasLost = lost}
            else 
               do let um = revealTile userMinefield minefield p
                  if checkIfWon userMinefield minefield then print "Congrats, you won"
                  else play Game {userM = um, logicM = minefield, hasWon = won, hasLost = lost}
      else 
         do putStr "Invalid input"
            play Game {userM = userMinefield, logicM = minefield, hasWon = won, hasLost = lost}



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

askForConfirmation :: IO (Char)
askForConfirmation = do putStrLn "Are you sure you want to reveal a flag? ('y'/'n')"
                        char <- getChar
                        return char