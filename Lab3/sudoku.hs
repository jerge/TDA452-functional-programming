module Sudoku where

import Test.QuickCheck
import Data.Maybe (isNothing)
import Data.Char
import Data.List

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving (Eq, Show)


-- A sample sudoku
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- A1
-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing) }

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku cs) | length cs == 9 = isSudokuRow (Sudoku cs)
                     | otherwise = False


isSudokuRow :: Sudoku -> Bool
isSudokuRow (Sudoku [])                     = True
isSudokuRow (Sudoku (r:rs)) | length r == 9 = isSudokuRow (Sudoku rs)
                            | otherwise     = False

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku [])   = True
isFilled (Sudoku (r:rs)) = isFilledRow r && isFilled (Sudoku rs)

isFilledRow :: [Maybe Int] -> Bool
isFilledRow []                    = True
isFilledRow (r:rs) | isNothing r  = False
                   | otherwise    = isFilledRow rs

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku (r:rs)) | rs /= [] = do putStrLn ((map $ maybe '.' intToDigit) r)
                                            printSudoku (Sudoku rs)
                            | otherwise = putStrLn ((map $ maybe '.' intToDigit) r)

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do file <- readFile path
                     return (Sudoku (map (map charToMaybeInt) (lines file)))

charToMaybeInt :: Char -> Maybe Int
charToMaybeInt '.' = Nothing
charToMaybeInt c = Just (digitToInt c )
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1, elements [Just n | n <- [1..9]]),
                  (9, return Nothing)]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------------------------------------------

type Block = [Maybe Int]


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:rs) = isOkayBlock rs
isOkayBlock (r:rs) = r `notElem` rs && isOkayBlock rs


-- * D2

blocks :: Sudoku -> [Block]
blocks s = rows s  ++ transpose (rows s) ++ squareBlocks (rows s)

squareBlocks :: [Block] -> [Block]
squareBlocks [] = []
squareBlocks s = squareBlock (transpose (take 3 s)) ++ squareBlocks (drop 3 s)

squareBlock :: [Block] -> [Block]
squareBlock [] = []
squareBlock s = concat (take 3 s):squareBlock (drop 3 s)

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length (blocks s) == 27 && prop_block_cells (rows s)

prop_block_cells :: [Block] -> Bool
prop_block_cells = foldr (\b -> (&&) (length b == 9)) True

-- * D3

isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku s) = blanks' s (0,0)

blanks' :: [[Maybe Int]] -> Pos -> [Pos]
blanks' [] _ = []
blanks' (c:cs) (l,r) = blankInRow c (l,r) ++ blanks' cs (l+1,r)

blankInRow :: [Maybe Int] -> Pos -> [Pos]
blankInRow [] _ = []
blankInRow (c:cs) (l,r) = if c == Nothing then (l,r) : rest else rest
          where rest = blankInRow cs (l,r+1)

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


-- * E4

candidates :: Sudoku -> Pos -> [Int]
candidates = undefined

--prop_candidates_correct :: ...
--prop_candidates_correct =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
