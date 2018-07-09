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

data Evaluation t = More t | Finished t

instance Functor Evaluation where
  fmap f (More t) = More (f t)
  fmap f (Finished t) = Finished (f t)

isNumericVal :: Term -> Bool
isNumericVal ZeroT = True
isNumericVal (SuccT t) = isNumericVal t
isNumericVal _ = False

evalStep :: Term -> Evaluation Term
evalStep t = case t of
  IfT TrueT t2 _ -> More t2
  IfT FalseT _ t3 -> More t3
  IfT t1 t2 t3 -> fmap (\t1' -> IfT t1' t2 t3) (evalStep t1)
  SuccT t1 -> fmap SuccT (evalStep t1)
  PredT ZeroT -> More ZeroT
  PredT (SuccT tt1) | isNumericVal tt1 -> More tt1
  PredT t1 -> fmap PredT (evalStep t1)
  IsZeroT ZeroT -> More TrueT
  IsZeroT (SuccT tt1) | isNumericVal tt1 -> More FalseT
  IsZeroT t1 | fmap IsZeroT (evalStep t1)
  _ -> Finished t

eval :: Term -> Term
eval t = case evalStep t of
  More t' -> eval t'
  Finished t' -> t'

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
