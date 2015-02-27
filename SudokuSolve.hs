module SudokuSolve where

import Solver
import Data.Char (intToDigit)
import Data.List (nub,transpose,elemIndex)
import Data.Set (Set)
import qualified Data.Set as Set

data SudokuConfig = SudokuConfig [Int] deriving Eq

instance Show SudokuConfig where
    show (SudokuConfig grid) = concat ("\n":[ row ++ "\n" | row <- rows ])
      where
        rows = [ formattedRow row | row <- partition 9 cells ]
        formattedRow r = concat [ (concat triple) ++ " " | triple <- partition 3 r ]
        cells = [ (representation cell):" " | cell <- grid ]
        representation cell | cell == 0 = '_'
                            | otherwise = intToDigit cell

instance Config SudokuConfig where
    successors config@(SudokuConfig grid) = fillCell nextBlankCell
      where
        nextBlankCell = elemIndex 0 grid
        fillCell Nothing      = []
        fillCell (Just index) =
          [
            let
              (ini, rest) = splitAt index grid
              newConfig = SudokuConfig (ini ++ (newValue:(tail rest)))
            in
              newConfig | newValue <- validValuesForCell config index ]

sudokuConfigFromList grid = SudokuConfig (map fromIntegral grid)

listFromSudokuConfig :: SudokuConfig -> [Int]
listFromSudokuConfig (SudokuConfig grid) = grid

sudokuSolve :: SudokuConfig -> (Maybe SudokuConfig)
sudokuSolve config = solve isSudokuGoal config

isSudokuGoal :: SudokuConfig -> Bool
isSudokuGoal config@(SudokuConfig grid) =
    noBlanks &&
      rowsSatisfy config &&
      columnsSatisfy config &&
      boxesSatisfy config
  where
    noBlanks = elemIndex 0 grid == Nothing

rowsSatisfy :: SudokuConfig -> Bool
rowsSatisfy config = and $ map nonupleIsGoal (allRows config)

columnsSatisfy :: SudokuConfig -> Bool
columnsSatisfy config = and $ map nonupleIsGoal (allColumns config)

boxesSatisfy :: SudokuConfig -> Bool
boxesSatisfy config = and $ map nonupleIsGoal (allBoxes config)

nonupleIsGoal :: [Int] -> Bool
nonupleIsGoal xs | length xs == 9 = sum xs == sum [1..9]

validValuesForCell :: SudokuConfig -> Int -> [Int]
validValuesForCell config index =
    Set.toList (possibleValues `Set.difference` (Set.unions [row, col, box]))
  where
    row = Set.fromList $ indexRow config index
    col = Set.fromList $ indexColumn config index
    box = Set.fromList $ indexBox config index
    possibleValues = Set.fromList [1..9]

validNonuple :: [Int] -> Bool
validNonuple xs | length xs == 9 = length (nub nonBlankCells) == length nonBlankCells
  where
    nonBlankCells = (filter (/= 0) xs)

indexRow ::SudokuConfig -> Int -> [Int]
indexRow config i | i >= 0 && i < 9 * 9 = row config (i `div` 9)
  where
    row (SudokuConfig grid) r | r >= 0 && r <= 9 = take 9 (drop (r * 9) grid)

indexColumn :: SudokuConfig -> Int -> [Int]
indexColumn config i | i >= 0 && i <= 9 * 9 = column config (i `rem` 9)
  where
    column config c | c >= 0 && c <= 9 = (allColumns config) !! c

indexBox :: SudokuConfig -> Int -> [Int]
indexBox (SudokuConfig grid) i | i >= 0 && i <= 9 * 9 = box (point i)
  where
    box (i, j) = [ grid !! (index (ii + offset i, jj + offset j)) | jj <- [0..2], ii <- [0..2] ]
    offset idx = 3 * (idx `div` 3)
    point index = (index - 9 * (index `div` 9), index `div` 9)
    index (i, j) = i + j * 9

allRows :: SudokuConfig -> [[Int]]
allRows (SudokuConfig grid) = partition 9 grid

allColumns :: SudokuConfig -> [[Int]]
allColumns config = transpose $ allRows config

allBoxes :: SudokuConfig -> [[Int]]
allBoxes config =
    map concat (concatMap (partition 3) (transpose (map (partition 3) (allRows config))))

partition :: Int -> [a] -> [[a]]
partition 0 xs = []
partition n [] = []
partition n xs = y:(partition n ys) where (y, ys) = splitAt n xs
