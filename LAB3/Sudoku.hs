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

-------------------------------------------------------------------------

type Pos = (Int,Int)

--given a Sudoku returns a list of the positions in the Sudoku that are still blank
blanks :: Sudoku -> [Pos]
blanks sud = foldr (++) [] [ index (((rows sud)!!i)!!j) i j| i <- [0..8], j <- [0..8]]
  where index Nothing k l = [(k,l)]
        index r _ _ = []

--given a list, and a tuple containing an index in the list and a new value,
--updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_, value) = [value]
(!!=) (x:xs) (0, value) = value:xs
(!!=) (x:xs) (index, value) = x:(xs !!= (index-1, value))

--property to test (!!=)
prop_updateValue :: [Maybe Int] -> (Int,Maybe Int) -> Bool
prop_updateValue [] (index, value) = [value] == [] !!= (index,value)
prop_updateValue oldList (index, value) = old1 == new1 && old2 == new2 &&
                                          newElement == value
  where newList = oldList !!= (index,value)
        (old1, _:old2) = splitAt index oldList
        (new1, newElement:new2) = splitAt index newList

--given a Sudoku, a position, and a new cell value, updates the given Sudoku at
--the given position with the new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update _ (k,l) _ | k<0 || l<0 = error "no negative positions"
update sud (k,l) val = Sudoku (rows sud !!= (k, updatedRow))
  where updatedRow = rows sud !! k !!= (l,val)

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update sud (k,l) val = rows (update sud (k,l) val) !! k !! l == val

--given a Sudoku, and a blank position, determines which numbers could be
--legally written into that position
candidates :: Sudoku -> Pos -> [Int]
candidates sud (k,l) | e /= Nothing = error "not a blank position"
  where e = (rows sud !! k) !! l
candidates sud (k,l) = [ x | x <- [1..9], isOkayEntry (Just x)]
  where isOkayEntry :: Maybe Int -> Bool
        isOkayEntry e = all (/= e) (rows sud !! k) &&
                        all (/= e) ((transpose $ rows sud) !! l ) &&
                        all (/= e) (getBlock sud k l)
        getBlock s i j = (drop 18 $ blocks s) !! (3*(i `div` 3)+j)

prop_candidates :: Sudoku -> Pos -> Bool
prop_candidates sud p = and [checkOkay sud p (Just x) |x <- candidates sud p]
                        && not (and [checkOkay sud p (Just x) |x <- notCand sud p])
  where notCand sud p = [x | x <- [1..9]] \\ candidates sud p
        checkOkay sud p val = isOkay (update sud p val) &&
                              isSudoku (update sud p val)

-------------------------------------------------------------------------

--returns the solution to a Sudoku or else Nothing
solve :: Sudoku -> Maybe Sudoku
solve sud | not (isSudoku sud) || not (isOkay sud) = Nothing
          | isSolved sud                           = Just sud
          | otherwise                              = head (solve' sud )

  where solve' s | isSolved s = [Just s]
                 | otherwise  = [solution | c <- candidates s (k,l)
                                  , solution <- solve' $ update s (k,l) (Just c)]
         where (k,l) = head (blanks s)
