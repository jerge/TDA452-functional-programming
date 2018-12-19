module Utils where

import Minesweeper

import Data.Char
import Data.List
import System.Random

-- Adds the elements of a tuple of tuples into new tuples
addPos :: (Pos,Pos) -> Pos
addPos ((x,y),(cx,cy)) = (x+cx,y+cy)

-- Checks if a position is inside the height and width,
-- as well as being larger than 0
validPos :: Pos -> Pos -> Bool
validPos (h,w) (r,c) = r >= 0 && c >= 0 && c < w && r < h

-- Returns the tile at the position
getTileAtPos :: Minefield -> Pos -> Tile
getTileAtPos (Minefield m) (r,c) = (m !! r) !! c

-- Updates the value of an index in a list
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | null xs
            || length xs <= i 
            || i < 0     = error "Index out of bounds"
             | otherwise = l++(y:rs)
    where (l,r:rs) = splitAt i xs

-- Updates at the specified location how many adjacent mines there are
updateNumberAtPos :: Minefield -> Minefield -> Pos -> Minefield
updateNumberAtPos userM m p = update userM p (adjacentMines m p)

-- Update a minefield at a position with a specified tile
update :: Minefield -> Pos -> Tile -> Minefield
update (Minefield m) (r,c) y = Minefield (m !!= (r,(m !! r) !!= (c,y)))

-- Calculates how many mines there are adjacent to the specified position
adjacentMines :: Minefield -> Pos -> Tile
adjacentMines m p | n == 0 = Empty
                       | otherwise = Num n
    where 
        n = length (filter isMine (map (getTileAtPos m) (adjacentPos (h,w) p)))
        h = length (rows m)
        w = length (head (rows m))

-- Function to return all 8 adjacent positions
allAdjacentPos :: Pos -> [Pos]
allAdjacentPos (x, y) = [ (x + dx, y + dy) | dx <- [-1, 0, 1],
                                              dy <- [-1, 0, 1],
                                              not (dx == 0 && dy == 0) ]
-- Returns all valid adjacent positions of a position
adjacentPos :: Pos -> Pos -> [Pos]
adjacentPos (h,w) p = filter (validPos (h,w)) (allAdjacentPos p)

-- Reveals a position
-- If it's an empty location it reveals it and calls revealEmpty
-- If it's a number then it reveals it
-- If it's a mine the player loses
revealTile :: Game -> Pos -> Game
revealTile g p | adjacentMines (logicM g) p == Empty 
                    = revealEmpty newGame p
               | getTileAtPos (logicM g) p /= Mine        
                    = newGame
               | otherwise                                
                    = Game {userM = userM g,
                            logicM = logicM g,
                            hasWon = hasWon g,
                            hasLost = True}
    where newGame = Game {userM   = updateNumberAtPos (userM g) (logicM g) p,
                            logicM  = logicM g,
                            hasWon  = hasWon g,
                            hasLost = hasLost g}

-- Returns a minefield with all adjacent empty and number tiles,
-- to the inputed empty position, revealed
-- This can definitely be done smoother, 
-- but I have already fiddled with it for an unreasonable amount of time
revealEmpty :: Game -> Pos -> Game
revealEmpty g p | isNum (getTileAtPos (logicM g) p) = revealTile g p
                | length unknowns > 3 = re3
                | length unknowns > 2 = re2
                | length unknowns > 1 = re1
                | unknowns /= [] = re0
                | otherwise = Game {userM = userM g, logicM = logicM g, 
                                    hasWon = False, hasLost = False}
    where 
        unknowns = adjacentUnknownZeros (userM g) (logicM g) p
        re0 = revealTile Game {userM    = update (userM g) (head unknowns) Empty,
                                logicM  = logicM g,
                                hasWon  = hasWon g,
                                hasLost = hasLost g} (head unknowns)
        re1 = revealTile re0 (unknowns !! 1)
        re2 = revealTile re1 (unknowns !! 2)
        re3 = revealTile re2 (unknowns !! 3)

-- Returns a list of positions of all the unknown adjacent empty tiles
adjacentUnknownZeros :: Minefield -> Minefield -> Pos -> [Pos]
adjacentUnknownZeros um m p = filter (not . isMine . adjacentMines m) $
                 filter (isUnknown . getTileAtPos um) $ adjacentPos (h,w) p
    where h = length (rows m)
          w = length (head (rows m))

isZero :: Tile -> Bool
isZero t = t == Num 0

-- Returns an array of all unknown positions
unknowns :: Minefield -> [Pos]
unknowns (Minefield rows) =
    [ (rowNum, colNum) | (rowNum, row) <- zip [0..] rows, 
                        (colNum, cell) <- zip [0..] row
                        , isUnknown cell ]

shuffle :: Eq a => StdGen -> [a] -> [a]
shuffle gen ls = fst (shuffle' gen ls [])

-- Shuffles a list by taking random elements from the original list and
-- inserting them into the end of a new list
shuffle' :: Eq a => StdGen -> [a] -> [a] -> ([a], StdGen)
shuffle' g [] new = (new,g)
shuffle' g old new = shuffle' g' (delete newElement old) (newElement : new)
        where 
            newElement = old !! i
            (i, g') = randomR (0,length old-1) g

-- Generates a random Minefield given a StdGen width height and amount of mines
randomMinefield :: StdGen -> Int -> Int -> Int -> Minefield
randomMinefield g w h nm | w*h < nm  = error "Too many mines"
                         | otherwise = randomMinefield' w h shuffledList [[]]
    where
        shuffledList = shuffle g (replicate nm Mine ++ replicate (w*h-nm) Empty)

randomMinefield' :: Int -> Int -> [Tile] -> [[Tile]] -> Minefield
randomMinefield' w h [] matrix = Minefield (take h matrix)
randomMinefield' w h ts matrix = randomMinefield' w h (drop w ts) (take w ts : matrix)
