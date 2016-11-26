module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | x<-[1..9]] | y<-[1..9]]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku matrix) = length matrix == 9 &&
                           all (\x -> length x == 9) matrix &&
                           all (\x -> all (\y -> ((Just 1<=y) && (y<=Just 9))
                            || (y==Nothing)) x) matrix



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
                  if (isSudoku sud) then return (sud)
                    else  error "error: This is not a Sudoku"

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
type Block = [Maybe Int]

--given a block, checks if that block does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock b = length (nonothing b) == length (nub (nonothing b))
  where nonothing b = filter isJust b

--given a Sudoku, creates a list of all blocks of that Sudoku
blocks :: Sudoku -> [Block]
blocks sud =  [ getBlocks (rows sud) j i | i <- [0..2], j <- [0..2]] ++
              rows sud ++ (transpose $ rows sud)
  where getBlocks r x y = foldr (++) [] (map (take 3 . drop (3*x)) (take 3 $ drop (3*y) (rows sud)))

--given a Soduko, checks that all rows, colums and 3x3 blocks do not contain
--the same digit twice
isOkay :: Sudoku -> Bool
isOkay sud = all (\x -> isOkayBlock x) (blocks sud)
