module SudokuSolve where

import Solver
import Data.Char (intToDigit)
import Data.List (nub,transpose,elemIndices)

data SudokuConfig = SudokuConfig [Int] deriving Eq

instance Show SudokuConfig where
    show (SudokuConfig grid) = concat [ row ++ "\n" | row <- rows ]
      where
        rows = [ formattedRow row | row <- partition 9 cells ]
        formattedRow r = concat [ (concat triple) ++ " " | triple <- partition 3 r ]
        cells = [ (representation cell):" " | cell <- grid ]
        representation cell | cell == 0 = '_'
                            | otherwise = intToDigit cell

instance Config SudokuConfig where
    successors = scs

scs :: SudokuConfig ->  [SudokuConfig]
scs (SudokuConfig grid) =
  filter validConfig (
    map SudokuConfig [
      let
        (ini, rest) = splitAt i grid
      in
        ini ++ (newValue:(tail rest)) | i <- elemIndices 0 grid,  newValue <- [1..9] ])

sudokuConfigFromList :: [Integer] -> SudokuConfig
sudokuConfigFromList grid = SudokuConfig (map fromIntegral grid)

listFromSudokuConfig :: SudokuConfig -> [Int]
listFromSudokuConfig (SudokuConfig grid) = grid

sudokuSolve :: SudokuConfig -> (Maybe SudokuConfig)
sudokuSolve config = solve isSudokuGoal config

isSudokuGoal :: SudokuConfig -> Bool
isSudokuGoal config = squaresStatisfy config && rowsSatisfy config && columnsSatisfy config

rowsSatisfy :: SudokuConfig -> Bool
rowsSatisfy config = and $ map nonupleIsGoal (allRows config)

columnsSatisfy :: SudokuConfig -> Bool
columnsSatisfy config = and $ map nonupleIsGoal (allColumns config)

squaresStatisfy :: SudokuConfig -> Bool
squaresStatisfy config = and $ map nonupleIsGoal (allColumns config)

nonupleIsGoal :: [Int] -> Bool
nonupleIsGoal xs | length xs == 9 = sum xs == sum [1..9]

validConfig :: SudokuConfig -> Bool
validConfig config = and $ map validNonuple allNonuples
  where
    allNonuples = (allRows config) ++ (allColumns config) ++ (allSquares config)

validIndex :: SudokuConfig -> Int -> Bool
validIndex (SudokuConfig grid) index =
    and $ map validNonuple [indexRow index, indexColumn index, indexSquare index]

validNonuple :: [Int] -> Bool
validNonuple xs | length xs == 9 = length (nub nonBlankCells) == length nonBlankCells
  where
    nonBlankCells = (filter (/= 0) xs)

row :: SudokuConfig -> Int -> [Int]
row (SudokuConfig grid) r | r >= 0 && n <= 9 = take 9 (drop (r * 9) grid)

column :: SudokuConfig -> Int -> [Int]
column config c | c >= 0 && c <= 9 = (allColumns config) !! c

square :: SudokuConfig -> Int -> [Int]
square config s | s >= 0  && s <= 9 = (allSquares config) !! s

indexRow ::SudokuConfig -> Int -> [Int]
indexRow config i | i >= 0 && i < 9 * 9 = row config (i `div` 9)

indexColumn :: SudokuConfig -> Int -> [Int]
indexColumn config i | i >= 0 && i <= 9 * 9 = column config (i `rem` 9)

indexSquare :: SudokuConfig -> Int -> [Int]
indexSquare config i | i >= 0 && i <= 9 * 9 = column config i

allRows :: SudokuConfig -> [[Int]]
allRows (SudokuConfig grid) = partition 9 grid

allColumns :: SudokuConfig -> [[Int]]
allColumns config = transpose $ allRows config

allSquares :: SudokuConfig -> [[Int]]
allSquares config =
    map concat (concatMap (partition 3) (transpose (map (partition 3) (allRows config))))

partition :: Int -> [a] -> [[a]]
partition 0 xs = []
partition n [] = []
partition n xs = y:(partition n ys) where (y, ys) = splitAt n xs

