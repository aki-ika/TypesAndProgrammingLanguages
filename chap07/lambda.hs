import           Control.Exception

data NoRuleApplies = NoRuleApplies deriving (Show)
instance Exception NoRuleApplies

data Term = TmVar Int Int
            | TmAbs String Term
            | TmApp Term Term
            deriving (Show, Eq)

printTm ctx t = case t of
        TmAbs x t1 -> let (ctx', x') = pickFreshName ctx x in
            "(lambda " ++ x' ++ ". " ++ printTm ctx' t1 ++ ")"
        TmApp t1 t2 -> "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
        TmVar fi x -> if length ctx == x then indexToName fi ctx x else "[bad index]"
        where
            indexToName fi ctx x = snd (ctx !! x)
            pickFreshName ctx x = let fi = length ctx in
                if x `elem` map snd ctx then pickFreshName ctx (x ++ "'") else (ctx ++ [(fi, x)], x)
data Binding = NameBind deriving (Show, Eq)
type Context = [(String, Binding)]

termShift d t = walk 0 t
    where
        walk c t = case t of
            TmVar fi x  -> if x >= c then TmVar fi (x + d) else TmVar fi x
            TmAbs x t1  -> TmAbs x (walk (c + 1) t1)
            TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

termSubst j s t = walk 0 t
    where
        walk c t = case t of
            TmVar fi x  -> if x == j + c then termShift c s else TmVar fi x
            TmAbs x t1  -> TmAbs x (walk (c + 1) t1)
            TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal ctx t = case t of
    TmAbs _ _ -> True
    _         -> False

eval1 ctx t = case t of
    TmApp (TmAbs x t12) v2 | isVal ctx v2 -> termSubstTop v2 t12
    TmApp v1 t2 | isVal ctx v1 -> let t2' = eval1 ctx t2 in TmApp v1 t2'
    TmApp t1 t2 -> let t1' = eval1 ctx t1 in TmApp t1' t2
    _ -> throw NoRuleApplies

eval ctx t = do
    t' <- try (evaluate (eval1 ctx t))
    case t' of
        Left NoRuleApplies -> return t
        Right t'           -> if t' == t then return t else eval ctx t'

main :: IO ()
main = do
    let term = TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "y" (TmVar 0 0))
    print term
    eval [] term >>= print
