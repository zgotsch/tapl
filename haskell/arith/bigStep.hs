-- Terms
data Term =
  TrueT |
  FalseT |
  IfT Term Term Term |
  ZeroT |
  SuccT Term |
  PredT Term |
  IsZeroT Term deriving Show

pretty :: Term -> String
pretty TrueT = "true"
pretty FalseT = "false"
pretty ZeroT = "0"
pretty (SuccT t1) | isNumericVal t1 = show $ 1 + read (pretty t1)
pretty t = "stuck \"" ++ show t ++ "\""

isNumericVal :: Term -> Bool
isNumericVal ZeroT = True
isNumericVal (SuccT t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal TrueT = True
isVal FalseT = True
isVal t | isNumericVal t = True
        | otherwise = False

eval :: Term -> Term
eval (IfT t1 t2 t3)
  | TrueT <- eval t1 = eval t2
  | FalseT <- eval t1 = eval t3
eval (SuccT t1) | isNumericVal (eval t1) = SuccT (eval t1)
-- replace the above with `eval (SuccT t1) = SuccT (eval t1)` to bring it in
-- line with the small-step evaluator
eval (PredT t1) | ZeroT <- eval t1 = ZeroT
eval (PredT t1) | SuccT nv1 <- eval t1 = nv1
eval (IsZeroT t1) | ZeroT <- eval t1 = TrueT
eval (IsZeroT t1) | SuccT nv1 <- eval t1 = FalseT
-- this covers the values
eval t = t

testTerms = [
  IfT TrueT ZeroT (SuccT ZeroT),
  IfT FalseT ZeroT (SuccT ZeroT),
  IfT (IsZeroT ZeroT) (SuccT ZeroT) ZeroT,
  IsZeroT ZeroT,
  SuccT (PredT (SuccT ZeroT)),
  SuccT (IsZeroT (SuccT ZeroT))
  ]

main :: IO ()
main = mapM_ (
  \t -> do
    putStrLn $ show t ++ " -> " ++ (pretty $ eval t)
  ) testTerms
