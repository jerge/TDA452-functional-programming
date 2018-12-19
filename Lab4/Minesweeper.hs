-- This is a haskell file
module Minesweeper where

import Test.QuickCheck

data Tile = Unknown | Flag | Num Int | Mine | Empty
    deriving (Eq, Show)
-- A minefield either represents the logic state (Mines or empty)
-- or it can represent what the user sees (any tile)
newtype Minefield = Minefield { rows :: [[Tile]]}
    deriving (Eq, Show)

data Game = Game {userM :: Minefield, logicM :: Minefield, 
                  hasWon :: Bool, hasLost :: Bool}
    deriving Show

instance Arbitrary Tile where
    arbitrary =
        do n <- choose(1, 8)
           elements [Unknown, Empty, Num n, Mine, Flag]

cell :: Int -> Int -> Gen Tile
cell mines tiles = frequency [(mines, return Mine),
                              (tiles, return Empty)]

instance Arbitrary Minefield where
    arbitrary =
        do height <- choose (1,30)  --Set height at atleast one row
           width <- choose (2,30)   --Set width between 2 and 30
           mines <- choose (1, width*height-1) -- Probability to get mines
           mf <- vectorOf height (vectorOf width (cell mines (width*height)))
           return (Minefield mf)

-- Creates a minefield of @w width and @h height with only Unknown
allUnknowns :: Int -> Int -> Minefield
allUnknowns w h= Minefield [[Unknown | x <- [1..w]] | x <- [1..h]]

-- Creates a minefield of @w width and @h height with only Empty
allEmpty :: Int -> Int -> Minefield
allEmpty w h = Minefield [[Empty | x <- [1..w]] | x <- [1..h]]

-- Returns the amount of a specific tile in the Minefield
amountTile :: Minefield -> Tile -> Int
amountTile (Minefield m) t = length $ filter (== t) (concat m)

-- Checks so the minefield has equally long rows
isMinefield :: Minefield -> Bool
isMinefield (Minefield (m:ms)) = all (isMinefield' (length m)) ms

isMinefield' :: Int -> [Tile] -> Bool
isMinefield' l ts = length ts == l

isMine :: Tile -> Bool
isMine Mine = True
isMine _ = False

isEmpty :: Tile -> Bool
isEmpty Empty = True
isEmpty _ = False

isUnknown :: Tile -> Bool
isUnknown Unknown = True
isUnknown _      = False

isNum :: Tile -> Bool
isNum (Num _) = True
isNum _ = False

type Pos = (Int, Int)
