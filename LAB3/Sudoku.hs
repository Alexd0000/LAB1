module Sudoku where

import Test.QuickCheck
import Data.Char

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

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

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | x<-[1..9]] | y<-[1..9]]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku matrix) = length matrix == 9 &&
                           all (\x -> length x == 9) matrix &&
                           all (\x -> all (\y -> ((Just 1<=y) && (y<=Just 9)) || (y==Nothing)) x) matrix



-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = isSudoku sud &&
                           all (\x -> all (\y -> y /= Nothing) x) (rows sud)

-------------------------------------------------------------------------

--converts the entry of a Sudoku field to a char, either the number or .
convertSudokuEntryToChar :: Maybe Int -> Char
convertSudokuEntryToChar Nothing = '.'
convertSudokuEntryToChar (Just n) = chr (n+48)

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = sequence_ (map putStrLn (map (map convertSudokuEntryToChar) (rows sud)))

--converts a char to the corresponding Sudoku Entry
convertCharToSudokuEntry :: Char -> Maybe Int
convertCharToSudokuEntry '.' = Nothing
convertCharToSudokuEntry c = Just ((ord c)-48)


-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku

readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
                  s <- readFile path
                  let sud = Sudoku (map (map convertCharToSudokuEntry) (lines(s)))
                  if (isSudoku sud) then return (sud) else  error "error: This is not a Sudoku"

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1,rNumber),(9,nothing)]
   where rNumber = elements [ (Just n)|n<-[1..9]]
         nothing = elements [Nothing]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

--property to check whether generated Sudoku is really a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
-------------------------------------------------------------------------
