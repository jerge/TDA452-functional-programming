module Sudoku where

import Test.QuickCheck
import Data.Maybe (isNothing)
import Data.Char

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
printSudoku (Sudoku (r:rs)) | rs /= [] = do print ((map $ maybe '.' intToDigit) r)
                                            printSudoku (Sudoku rs)
                            | otherwise = print ((map $ maybe '.' intToDigit) r)

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

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
isOkayBlock = undefined


-- * D2

blocks :: Sudoku -> [Block]
blocks = undefined

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

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
