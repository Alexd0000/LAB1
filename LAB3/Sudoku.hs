module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe


example :: Sudoku
example =
  Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

exampleBis :: Sudoku
exampleBis =
  Sudoku
    [ [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    ]

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

-- Function that given a Sudoku returns a list of the positions in the Sudoku that are still blank
blanks :: Sudoku -> [Pos]
blanks (Sudoku sud) = [(i,j) | i <- [0..8], j<-[0..8], (sud!!i!!j) == Nothing]

-- Property that states that all cells in the blanks list are actually blank
prop_blanks:: Sudoku -> Bool
prop_blanks sud = all (\tuple -> (rows sud)!!(fst tuple)!!(snd tuple) == Nothing ) (blanks sud)


--Function that given a list, and a tuple containing an index in the list and a new value,
--updates the given list with the new value at the given index.
-- If the initial list is empty we throw an error or same if the index is negative or higher than its length
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_,_) = error "The list is empty"
(!!=) (x:xs) (index,value) | index<0 || index > (length (x:xs)-1) = error "Invalid index"
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
update (Sudoku sud) (k,l) val = Sudoku (sud !!= (k, updatedRow))
  where updatedRow = (sud!!k) !!= (l,val)

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
solve sud | (not (isSudoku sud)) || (not (isOkay sud)) = Nothing
          | otherwise = solve' sud (blanks sud)

-- Solve helper filling in all the blanks
solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' sud [] = Just sud
solve' sud (x:xs) = tryToSolveCell sud x can
  where can = candidates sud x



-- Solves a cell at a position with a candidate
tryToSolveCell :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
tryToSolveCell _ _ [] = Nothing
tryToSolveCell sud pos (x:xs) | isNothing solveNext = tryToSolveCell sud pos xs
                            | otherwise = solveNext
  where updated = update sud pos (Just x)
        solveNext = solve updated






