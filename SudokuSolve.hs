module SudokuSolve where

import Solver
import Data.Char (intToDigit)
import Data.List (transpose)

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
    successors (SudokuConfig grid) = []

sudokuConfigFromList :: [Integer] -> SudokuConfig
sudokuConfigFromList grid = SudokuConfig (map fromIntegral grid)

listFromSudokuConfig :: SudokuConfig -> [Int]
listFromSudokuConfig (SudokuConfig grid) = grid

isSudokuGoal :: SudokuConfig -> Bool
isSudokuGoal config = squaresAreGoal config && rowsAreGoal config && columnsAreGoal config

rowsAreGoal :: SudokuConfig -> Bool
rowsAreGoal config = sum (map sum (sudokuGridRows config)) == 9 * 9

columnsAreGoal :: SudokuConfig -> Bool
columnsAreGoal config = sum (map sum (sudokuGridColumns config)) == 9 * 9

squaresAreGoal :: SudokuConfig -> Bool
squaresAreGoal config = False

setIsGoal :: [Int] -> Bool
setIsGoal xs = sum xs == 45

sudokuSolve :: SudokuConfig -> (Maybe SudokuConfig)
sudokuSolve _ = Nothing

row :: SudokuConfig -> Int -> [Int]
row (SudokuConfig grid) n | n <= 9 = take 9 (drop (n * 9) grid)

column :: SudokuConfig -> Int -> [Int]
column config n | n <= 9 = (sudokuGridColumns config) !! n

square :: SudokuConfig -> Int -> [Int]
square config n | n <= 9 = (sudokuGridSquares config) !! n

partition :: Int -> [a] -> [[a]]
partition 0 xs = []
partition n [] = []
partition n xs = y:(partition n ys) where (y, ys) = splitAt n xs

sudokuGridRows :: SudokuConfig -> [[Int]]
sudokuGridRows (SudokuConfig grid) = partition 9 grid

sudokuGridColumns :: SudokuConfig -> [[Int]]
sudokuGridColumns config = transpose (sudokuGridRows config)

sudokuGridSquares :: SudokuConfig -> [[Int]]
sudokuGridSquares config =
      map concat (concatMap (partition 3) (transpose (map (partition 3) (sudokuGridRows config))))
