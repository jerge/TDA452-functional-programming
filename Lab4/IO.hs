module IO where

import Minesweeper
import Utils

import Data.Char
import System.Random

printMinefield :: Minefield -> IO ()
printMinefield (Minefield ms) = 
   do putStr "\n    0 "
      putStrLn $ concatMap showIndexCol [1..width-1]
      mapM_ printRow (zip ms [0..height-1])
   where width = length (head ms)
         height = length ms

showIndexCol :: Int -> String
showIndexCol i | i < 10 = ' ' : show i ++ " "
               | otherwise = show i ++ " "

printRow :: ([Tile],Int) -> IO ()
printRow (ms,r) = putStrLn (showIndexCol r ++ concatMap tileToChar ms)

tileToChar :: Tile -> String
tileToChar Unknown   = "[#]"
tileToChar Mine      = "[m]"
tileToChar Empty     = " ■ "
tileToChar (Num 0)   = "[0]"
tileToChar (Num i)   = " " ++ intToDigit i : " "
tileToChar Flag      = "[X]"

main :: IO ()
main = 
   do putStrLn "What width do you want?"
      w <- readLn
      putStrLn "What height do you want?"
      h <- readLn
      putStrLn "How many mines do you want?"
      nm <- readLn
      gen <- newStdGen
      let game = Game {userM = allUnknowns w h, 
                       logicM = randomMinefield gen w h nm, 
                       hasWon = False, 
                       hasLost = False}
      printMinefield (randomMinefield gen w h nm)
      play game


play :: Game -> IO ()
play Game {hasWon = True} = print "Congrats! You Won!"
play Game {hasLost = True} = print "You lost"
play Game {userM = userMinefield, logicM = minefield, 
           hasWon = won, hasLost = lost} = 
   do printMinefield userMinefield
      putStrLn "Enter a token ('f' to set flag, 's' to select or 'q' to quit"
      char <- getChar
      if char == 'q' then print "Bye Bye"
      else if char == 'f' then
         do p <- askForPos
            play Game {userM = update userMinefield p Flag, 
                       logicM = minefield, 
                       hasWon = won, hasLost = lost}
      else if char == 's' then 
         do p <- askForPos
            if getTileAtPos userMinefield p == Flag then
               do c <- askForConfirmation
                  if 'y' /= c then play Game {userM = userMinefield, 
                                              logicM = minefield, 
                                              hasWon = won, hasLost = lost}
                  else 
                     do let newGame = revealTile Game {userM = userMinefield,
                                                       logicM = minefield,
                                                       hasWon = won,
                                                       hasLost = lost} p
                        play $ checkIfWon newGame
            else 
               do let newGame = revealTile Game {userM   = userMinefield,
                                                 logicM  = minefield,
                                                 hasWon  = won,
                                                 hasLost = lost} p
                  play $ checkIfWon newGame
      else 
         do putStr "Invalid input"
            play Game {userM = userMinefield, logicM = minefield, 
                       hasWon = won, hasLost = lost}



checkIfWon :: Game -> Game
checkIfWon g = Game {userM = userM g,
                     logicM = logicM g,
                     hasWon = amountTile (userM g) Unknown
                              + amountTile (userM g) Flag
                              == amountTile (logicM g) Mine,
                     hasLost = hasLost g}


askForPos :: IO Pos
askForPos = 
   do putStr "\nEnter row: "
      r <- readLn
      putStr "Enter col: "
      c <- readLn
      return (r,c)

askForConfirmation :: IO Char
askForConfirmation = 
   do putStrLn "Are you sure you want to reveal a flag? ('y'/'n')"
      getChar
