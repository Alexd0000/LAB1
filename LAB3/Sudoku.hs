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

--Returns True if value is between min and max
between :: Ord a => a -> a -> a -> Bool
between value min max = value >= min && value <= max

--property to test (!!=)
prop_updateValue :: [Maybe Int] -> (Int,Maybe Int) -> Property
prop_updateValue oldList (index, value) = (oldList /= [] && between index 0 (length oldList -1))
                    ==> old1 == new1 && old2 == new2 && newElement == value
  where newList = oldList !!= (index,value)
        (old1, _:old2) = splitAt index oldList
        (new1, newElement:new2) = splitAt index newList

--given a Sudoku, a position, and a new cell value, updates the given Sudoku at
--the given position with the new value

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update _ (k,l) _ | k<0 || l<0 = error "no negative positions"
update (Sudoku sud) (k,l) val = Sudoku (sud !!= (k, updatedRow))
  where updatedRow = (sud!!k) !!= (l,val)

prop_update :: Sudoku -> Pos -> Maybe Int -> Property
prop_update sud (k,l) val = (between k 0 8 && between l 0 8) ==>
                            rows (update sud (k,l) val) !! k !! l == val

--given a Sudoku, and a blank position, determines which numbers could be
--legally written into that position
--an error will be returned if the position is not a blank
candidates :: Sudoku -> Pos -> [Int]
candidates sud (k,l) | e /= Nothing = error "not a blank position"
  where e = (rows sud !! k) !! l
candidates sud (k,l) = [ x | x <- [1..9], isOkayEntry (Just x)]
  where isOkayEntry :: Maybe Int -> Bool
        isOkayEntry e = all (/= e) (rows sud !! k) &&
                        all (/= e) ((transpose $ rows sud) !! l ) &&
                        all (/= e) (getBlock sud k l)
        getBlock s i j = blocks s !! (3*(i `div` 3)+j `div`3)

prop_candidates :: Sudoku -> Pos -> Property
prop_candidates sud p = (isOkay sud && fst p >= 0 && snd p >= 0 && elem p (blanks sud))  ==>
                      and [checkOkay sud p (Just x) |x <- candidates sud p]
                      && [] == filter (==True) [checkOkay sud p (Just x) |x <- notCand sud p]
  where notCand sud p = [x | x <- [1..9]] \\ candidates sud p
        checkOkay sud p val = isOkay (update sud p val)

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


-- produces instructions for reading the Sudoku from the given file, solving it,
-- and printing the answer
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
                    sud <- readSudoku path
                    let solution = solve sud
                    if isJust solution then printSudoku $ fromJust solution
                      else putStrLn "(no solution)"

--checks, given two Sudokus, whether the first one is a solution (i.e. all
--blocks are okay, there are no blanks), and also whether the first one is a
--solution of the second one (i.e. all digits in the second sudoku are
--maintained in the first one).
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s p = isSolved s && isOkay s && solves (rows s) (rows p)
  where solves x y = and [ (x!!i!!j) == (y!!i!!j) | i <- [0..8],
                                                j <- [0..8], isJust (y!!i!!j) ]

--check if the function solve is sound
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isOkay sud ==> isSolutionOf (fromJust $ solve sud) sud

