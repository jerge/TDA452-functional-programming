-- | MonadicEvaluators
-- Exercises with arithmetic expressions and monads
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This is just a skeleton, the definitions will be filled in
during the lecture.
-}
--------------------------------------------------------------------------------

module MonadicEvaluators where
    import Data.Char(isDigit,isLetter)
    import Data.List(union)
    import Control.Monad(ap,liftM,guard)
    import Parsing
    
    -- | A Haskell data type for arithmetic expressions with variables and division
    data Expr = Num Integer
              | Var Name
              | Add Expr Expr
              | Mul Expr Expr
              | Div Expr Expr      -- new
              deriving (Eq)
    
    type Name = String
    
    ex1 = Num 2
    ex2 = Add (Num 2) (Num 2)
    ex3 = Mul (Add (Num 1) (Num 2)) (Num 3)
    ex4 = Add (Num 1) (Mul (Num 2) (Num 3))
    ex5 = Div x (Mul (Num 2) y)
    ex6 = Mul (Num 2) x
    ex7 = Add (Mul (Num 2) x) (Mul (Num 3) y)
    ex8 = Add (Mul (Num 2) (Mul x x)) (Mul (Num 3) y)
    
    x   = Var "x"
    y   = Var "y"
    
    --------------------------------------------------------------------------------
    
    -- Here is a different implementation of the Show instance for Expr,
    -- to illustrate the systematic treatment of operator precedences used in the
    -- Show class, which makes them work across Show instances for different
    -- types. Examples to try:
    --     > show ex2            -- ex2 is shown at precedence level 0
    --     > show (Just ex2)     -- ex2 is shown at precedence level 11
    --     > show ex5            -- 2*y is shown at precedence level 8
    
    instance Show Expr where
      showsPrec p e =
        case e of
          Num n     -> shows n
          Var x     -> (x++)
          Add e1 e2 -> showParen (p>6) (showsPrec 6 e1 . ('+':) . showsPrec 6 e2)
          Mul e1 e2 -> showParen (p>7) (showsPrec 7 e1 . ('*':) . showsPrec 7 e2)
          Div e1 e2 -> showParen (p>7) (showsPrec 7 e1 . ('/':) . showsPrec 8 e2)
    
    -- Warning: for testing in GHCi, it might be better to use deriving Show,
    -- so that you can see the difference between e.g. (1+2)+3 and 1+(2+3)
    
    --------------------------------------------------------------------------------
    -- * Parsing expressions with variables and division
    
    {- BNF:
    expr ::= term {"+" term}.
    term ::= factor {mulOp factor}.
    mulOp  ::= "*" | "/".
    factor ::= number | name | "(" expr ")".
    number ::= digit {digit}.
    name ::= letter {letter}.
    -}
    
    expr, term, factor :: Parser Expr
    
    expr = leftAssoc Add term (char '+')
    
    term = do f1 <- factor
              fs <- zeroOrMore (flip <$> mulOp <*> factor)
              return (foldl (flip id) f1 fs)
    
              -- f1 :: Expr
              -- fs :: [Expr -> Expr]
              -- flip id x f ==> id f x ==> f x
              -- foldl (flip id) :: Expr -> [Expr->Expr] -> Expr
    
    mulOp :: Parser (Expr->Expr->Expr)
    mulOp = char '*' *> return Mul
            <|>
            char '/' *> return Div
    
    factor =   Num <$> number
           <|> Var <$> name
           <|> char '(' *> expr <* char ')'
    
    name :: Parser Name
    name = oneOrMore (sat isLetter)
    
    number :: Parser Integer
    number = readsP   -- From the Parsing library: readsP :: Read a => Parser a
    
    
    leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
    leftAssoc op item sep = foldl1 op <$> chain item sep
    
    
    --------------------------------------------------------------------------------
    -- | Evaluating Symbolic Expressions, without error handling
    
    eval_v1 :: [(Name,Integer)] -> Expr -> Integer
    eval_v1 env (Num n) = n
    eval_v1 env (Var x) = case lookup x env of
                         Just n -> n
                         Nothing -> error ("undefined variable: "++x)
    eval_v1 env (Add e1 e2) = eval_v1 env e1 + eval_v1 env e2
    eval