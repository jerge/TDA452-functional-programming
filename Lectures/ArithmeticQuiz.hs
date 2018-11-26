-- | Arithmetic Quiz
-- An exercise with recursive data types
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This is just a skeleton, the definitions will be filled in
during the lecture.
-}

--------------------------------------------------------------------------------
import Control.Monad(forever)

import Test.QuickCheck

main = do putStrLn "Welcome to the arithmetic quiz!"
          forever quiz

quiz =  do e <- generate arbitrary
           putStr ("What is "++ showExpr e ++"?")
           answer <- readLn
           let correct = eval e
           if answer == correct then 
                putStrLn ("Yes, that is correct!")
           else 
                putStrLn ("No, the correct answer is " ++ show correct)

--------------------------------------------------------------------------------
-- | A representation of simple arithmetic expressions
data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
            deriving Show


ex1 = Num 2  -- 2
ex2 = Add (Num 1) (Num 2)  -- 1 + 2 
ex3 = Mul (Add (Num 1) (Num 2)) (Num 3)  -- (1+2)*3
ex4 = Add (Num 1) (Mul (Num 2) (Num 3))  -- 1+2*3


--------------------------------------------------------------------------------

-- | Evaluate (compute the value of) an expression
eval :: Expr -> Integer
eval (Num n)        = n
eval (Add e1 e2)    = eval e1 + eval e2
eval (Mul e1 e2)    = eval e1 * eval e2

--------------------------------------------------------------------------------

-- | Showing expressions
showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor :: Expr -> String
showFactor e@(Add e1 e2)    = "(" ++ showExpr e ++ ")"
showFactor e                = showExpr e

--instance Show Expr where 

--------------------------------------------------------------------------------
-- * Generating arbitrary expressions

-- | Random generator (bad control over size. And compilation error)
{-
rExpr_v1 :: Gen Expr
rExpr_v1 = oneof [do n <- choose (1,10)
                    return (Num n),
                  do op <- elements [Add,Mul]
                    e1 <- rExpr_v1
                    e2 <- rExpr_v1
                    return (op e1 e2)]
-}
-- Random generator with exact control of the number of operators
rExpr_v2 :: Int -> Gen Expr
rExpr_v2 0 = do n <- choose (1,10)
                return (Num n)
rExpr_v2 n = do op <- elements [Add,Mul]
    --l <- elements [0.. n-1]
                l <- choose (0,n-1)
                let r = n-1 - l
                e1 <- rExpr_v2 l
                e2 <- rExpr_v2 r
                return (op e1 e2)

instance Arbitrary Expr where arbitrary = do n <- choose  (1,3)
                                             rExpr_v2 n

