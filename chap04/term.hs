import           Control.Exception

data NoRuleApplies = NoRuleApplies deriving (Show)
instance Exception NoRuleApplies

data Term = TmTrue
            | TmFalse
            | TmIf  Term Term Term
            | TmZero
            | TmSucc  Term
            | TmPred  Term
            | TmIsZero  Term
            deriving (Show, Eq)

isNumericVal t = case t of
    TmZero    -> True
    TmSucc t1 -> isNumericVal t1
    _         -> False

isVal t = case t of
    TmTrue             -> True
    TmFalse            -> True
    t | isNumericVal t -> True
    _                  -> False


eval1 term = case term of
    TmIf TmTrue t2 t3 -> t2
    TmIf TmFalse t2 t3 -> t3
    TmIf t1 t2 t3 -> let t1' = eval1 t1 in TmIf t1' t2 t3
    TmSucc t1 -> let t1' = eval1 t1 in TmSucc t1'
    TmPred TmZero -> TmZero
    TmPred (TmSucc nv1) | isNumericVal nv1 -> nv1
    TmPred t1 -> let t1' = eval1 t1 in TmPred t1'
    TmIsZero TmZero -> TmTrue
    TmIsZero (TmSucc nv1) | isNumericVal nv1 -> TmFalse
    TmIsZero t1 -> let t1' = eval1 t1 in TmIsZero t1'
    _ -> throw NoRuleApplies

eval :: Term -> IO Term
eval term = do
    term' <- try (evaluate (eval1 term))
    case term' of
        Left NoRuleApplies -> return term
        Right term'        -> if term' == term then return term else eval term'

main :: IO ()
main = do
    let term = TmIsZero  (TmPred  (TmSucc  TmZero))
    print term
    eval term >>= print
