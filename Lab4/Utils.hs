module Utils where

import Minesweeper

import Data.Char

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

-- Updates at the specified location how many neighbouring mines there are
updateNumberAtPos :: Minefield -> Minefield -> Pos -> Minefield
updateNumberAtPos userM m p = update userM p (calcNeighbourCount m p)

-- Update a minefield at a position with a specified tile
update :: Minefield -> Pos -> Tile -> Minefield
update (Minefield m) (r,c) y = Minefield (m !!= (r,(m !! r) !!= (c,y)))

-- Calculates how many mines there are neighbouring the specified position
calcNeighbourCount :: Minefield -> Pos -> Tile
calcNeighbourCount m p | n == 0 = Empty
                       | otherwise = Num n
    where n = length (filter isMine (map (getTileAtPos m) (neighbourPos (height,width) p)))
          height = length (rows m)
          width = length (head (rows m))

-- Function to return all 8 neighbouring positions
allNeighbourPos :: Pos -> [Pos]
allNeighbourPos (x, y) = [ (x + dx, y + dy) | dx <- [-1, 0, 1],
                                              dy <- [-1, 0, 1],
                                              not (dx == 0 && dy == 0) ]
-- Returns all valid neighbouring positions of a position
neighbourPos :: Pos -> Pos -> [Pos]
neighbourPos (h,w) p = filter (validPos (h,w)) (allNeighbourPos p)

-- Reveals a position
-- If it's an empty location it reveals it and calls revealEmpty
-- If it's a number then it reveals it
-- If it's a mine the player loses
revealTile :: Minefield -> Minefield -> Pos -> Minefield
revealTile userMinefield minefield p | calcNeighbourCount minefield p == Empty = revealEmpty newM minefield p 
                                     | getTileAtPos minefield p /= Mine        = newM
                                     | otherwise                               = error "You lost"
    where newM = updateNumberAtPos userMinefield minefield p

-- Returns a minefield with all neighbouring empty and number tiles,
-- to the inputed empty position, revealed
-- This can definitely be done smoother, 
-- but I have already fiddled with it for an unreasonable amount of time
revealEmpty :: Minefield -> Minefield-> Pos -> Minefield
revealEmpty um m p | isNum (getTileAtPos m p) = revealTile um m p
                   | length unknowns > 3 = re3
                   | length unknowns > 2 = re2
                   | length unknowns > 1 = re1
                   | unknowns /= [] = re0
                   | otherwise = um
    where unknowns = (adjacentUnknownZeros um m p)
          re0 = revealTile (update um (unknowns !! 0) Empty) m (unknowns !! 0)
          re1 = revealTile re0 m (unknowns !! 1)
          re2 = revealTile re1 m (unknowns !! 2)
          re3 = revealTile re2 m (unknowns !! 3)

-- TODO plz fix
allAdjacentPos :: Pos -> [Pos]
allAdjacentPos (x, y) = [ (x + dx, y + dy) | dx <- [-1, 0, 1],
                                          dy <- [-1, 0, 1],
                                          dx + dy == 1 ]
adjacentPos :: Pos -> Pos -> [Pos]
adjacentPos (h,w) p = filter (validPos (h,w)) (allNeighbourPos p)

-- Hlint This
adjacentUnknownZeros :: Minefield -> Minefield -> Pos -> [Pos]
adjacentUnknownZeros um m p = filter (not . isMine . calcNeighbourCount m) (filter (isUnknown . (getTileAtPos um)) (adjacentPos (h,w) p))
    where h = length (rows m)
          w = length (head (rows m))
    
isZero :: Tile -> Bool
isZero t = t == Num 0
---------------------- Unused ------------------------

-- Returns an array of all unknown positions
unknowns :: Minefield -> [Pos]
unknowns (Minefield rows) =
    [ (rowNum, colNum) | (rowNum, row) <- zip [0..] rows, 
                        (colNum, cell) <- zip [0..] row
                        , isUnknown cell ]

