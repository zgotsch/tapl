import Data.Char (ord, chr)

data Term =
  VarT Int |
  AbsT Term |
  AppT Term Term deriving Show

pretty :: Term -> String
pretty (VarT i) = show i
pretty (AbsT t1) = "(λ." ++ pretty t1 ++ ")"
pretty (AppT t1 t2) = "(" ++ pretty t1 ++ " " ++ pretty t2 ++ ")"

data NamedTerm =
  VarN String |
  AbsN String NamedTerm |
  AppN NamedTerm NamedTerm

nextLetter :: String -> String
nextLetter "z" = "a"
nextLetter (c:_) = [chr (ord c + 1)]

nextName :: [String] -> String
nextName [] = "a"
nextName (last:_) = nextLetter last

restoreNames :: Term -> NamedTerm
restoreNames t = restoreNames' [] t

restoreNames' :: [String] -> Term -> NamedTerm
restoreNames' names (VarT i) = VarN (names !! i)
restoreNames' names (AbsT t1) = let newName = nextName names in
  AbsN newName (restoreNames' (newName:names) t1)
restoreNames' names (AppT t1 t2) = AppN
  (restoreNames' names t1)
  (restoreNames' names t2)

prettyNames' :: NamedTerm -> String
prettyNames' (VarN name) = name
prettyNames' (AbsN name t1) = "(λ" ++ name ++ "." ++ prettyNames' t1 ++ ")"
prettyNames' (AppN t1 t2) = "(" ++ prettyNames' t1 ++ " " ++ prettyNames' t2 ++ ")"

prettyNames :: Term -> String
prettyNames = prettyNames' . restoreNames

data Evaluation t = More t | Finished t

instance Functor Evaluation where
  fmap f (More t) = More (f t)
  fmap f (Finished t) = Finished (f t)

shift :: Int -> Int -> Term -> Term
shift d c (VarT i) | i < c = VarT i
                   | otherwise = VarT (i + d)
shift d c (AbsT t1) = AbsT $ shift d (c + 1) t1
shift d c (AppT t1 t2) = AppT (shift d c t1) (shift d c t2)

type Subs = (Int, Term)

subs :: Subs -> Term -> Term
subs (from, to) (VarT i) | i == from = to
                         | otherwise = VarT i
subs (from, to) (AbsT t1) = AbsT $
  subs (from + 1, shift 1 0 to) t1
subs s (AppT t1 t2) = AppT (subs s t1) (subs s t2)

isValue :: Term -> Bool
isValue (VarT _) = True
isValue (AbsT _) = True
isValue _ = False

evalStep :: Term -> Evaluation Term
evalStep (AppT t1 t2)
  | not $ isValue t1 = fmap (flip AppT t2) (evalStep t1)
  | isValue t1, not $ isValue t2 = fmap (AppT t1) (evalStep t2)
  | AbsT t1body <- t1, isValue t2 = More $
    shift (-1) 0 (subs (0, shift 1 0 t2) t1body)
evalStep t = Finished t


eval :: Term -> Term
eval t = case evalStep t of
  More t' -> eval t'
  Finished t' -> t'

zeroT = AbsT (AbsT (VarT 0))
succT = (
  AbsT (AbsT (AbsT (AppT
    (AppT
      (VarT 2)
      (VarT 1)
    )
    (AppT
      (VarT 1)
      (VarT 0)
    )
  )))
  )
testTerms = [
  -- VarT 0, -- free variable, crashes pretty names
  AbsT (VarT 0), -- id
  AppT
    (AbsT (VarT 0))
    (AppT
      (AbsT (VarT 0))
      (AbsT (AppT
        (AbsT (VarT 0))
        (VarT 0)
      ))
    ), -- reduction example
  zeroT, -- zero
  AbsT (AbsT (AppT (VarT 1) (VarT 0))), --one
  AbsT (AbsT (AppT (VarT 1) (AppT (VarT 1) (VarT 0)))), --two
  succT,
  AppT succT zeroT
  ]

main :: IO ()
main = mapM_ (
  \t -> do
    putStrLn $ prettyNames t ++ " -> " ++ prettyNames (eval t)
  ) testTerms
