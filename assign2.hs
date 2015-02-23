module SudokuSolver where
    import Solver

data SudokuConfig = SudokuConfig grid deriving Eq

sudokuConfigFromList :: [Int] -> SudokuConfig
listFromSudokuConfig :: SudokuConfig -> [Int]


