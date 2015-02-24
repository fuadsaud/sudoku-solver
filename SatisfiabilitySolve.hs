module SatisfiabilitySolve where

data BExp = BConst Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp deriving (Eq,Show)

data SatConfig = SatConfig deriving Eq

instance Show SatConfig where
    show (SatConfig (BConst value))  = show value
    show (SatConfig (Var varName))   = varName
    show (SatConfig (And lexp rexp)) = (show lexp) ++ "||" ++ (show rexp)
    show (SatConfig (Or  lexp rexp)) = (show lexp) ++ "&&" ++ (show rexp)
    show (SatConfig (Not exp))       = '~':(show exp)

instance Config SatConfig where
    successors = []

isGoal :: SatConfig -> Bool
isGoal _ = False

satSolve :: BExp -> Maybe SatConfig
satSolve bexp = solve isGoal (SatConfig bexp)
