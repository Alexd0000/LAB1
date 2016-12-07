module StandardLab where

import Test.QuickCheck
import Parsing
import Data.Maybe
import Data.Char

data Expr = Num Double | Add Expr Expr | Mul Expr Expr | X | Function Name Expr

type Name = String

exExpr1 = Num 7											-- 7

exExpr2 = Add (Add X (Mul (Num 2) (Num 5))) (Num 2.7)   --  x + 2*5 + 2.7

exExpr3 = Mul (Num 2) (Function "sin" (Add (Num 2.3) X))				-- 2*sin(2.3+x)

exExpr4 = Mul (Add (Num 3) X) (Add (Num 2) (Num 2.5)) 	-- (3+x)*(2+2.5)

exExpr5 = Mul (Add (Mul (Num 2) (Function "sin" (Add (Num 2.3) X))) (Num 9.1)) (Add (Num 2.3) X) -- (2*sin(2.3+x)+9.1)*(2.3+x)

-- Function that converts any expression to string
{-
   Parentheses are required only in the following cases:
		1- When the arguments of a *-expression use +. For example: (3.1+4.2)*7
		2- When the argument of sin or cos uses * or +. For example: sin (3.2*x)
-}
showExpr :: Expr -> String
showExpr (Num f) = show f
showExpr X = "x"
showExpr (Function name e) = name++" "++ showFactorSin e
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactorMul e1 ++ "*" ++ showFactorMul e2

-- Helper function that add parenthesis for case 1 above
showFactorSin::Expr -> String
showFactorSin (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showFactorSin (Mul e1 e2) = "(" ++ showFactorMul e1 ++ "*" ++ showFactorMul e2 ++")"

-- Helper function that add parenthesis for case 2 above
showFactorMul:: Expr -> String
showFactorMul (Add e1 e2) =  "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showFactorMul e = showExpr e

-- Default show function by making Expr an instance of the class Show
instance Show Expr where
  show = showExpr

-- Function that, given an expression, and the value for the variable x, calculates the value of the expression
eval :: Expr -> Double -> Double
eval X valueVar = valueVar
eval (Num n) valueVar = n
eval (Function name e) valueVar | name=="sin" = sin (eval e valueVar)
                                | name== "cos" = cos (eval e valueVar)
                                | otherwise = error "Unknown function"
eval (Add e1 e2) valueVar = (eval e1 valueVar) + (eval e2 valueVar)
eval (Mul e1 e2) valueVar = (eval e1 valueVar) * (eval e2 valueVar)

-- Function that, given a string, tries to interpret the string as an expression, and returns Just of that expression if it succeeds. 
-- Otherwise, Nothing will be returned.
readExpr :: String -> Maybe Expr
readExpr = undefined



-- ======================================== TEST ZONE HAHAHAHAHAHHAHAHA =================================================

{- BNF:
test   ::= expr "name" test | expr



expr   ::= term "+" expr | term.
term   ::= factor "*" term | factor.
factor ::= "name" expr | factor'
factor' ::= unit | "(" expr ")".
unit ::= number | var

-}

-- | Parse a number which is integer (eg : 1 | 2 | 3)
numberWithoutDot :: Parser Double
numberWithoutDot = do s <- oneOrMore digit
                      return (read s :: Double)

-- | Parse a number which is double (eg : 1.9 | 23.8 | 5.333)
numberWithDot :: Parser Double
numberWithDot = do s1 <- oneOrMore digit
                   char '.'
                   s2 <- oneOrMore digit
                   return (read (s1++"."++s2) :: Double)

-- | Parse a number
number :: Parser Double
number = numberWithDot <|> numberWithoutDot


-- | Parse a name of a function
funcName :: Parser String
funcName = do s <- oneOrMore (sat isLower)
              char ' '
              return (s)

test, expr, term, factor, unit :: Parser Expr

expr = expr' <|> term
  where
    expr' = do t <- term
               char '+'
               e <- expr
               return (Add t e)

term = term' <|> factor
  where
    term' = do f <- test --f <- factor
               char '*'
               t <- term
               return (Mul f t)

test = test' <|> factor
  where
  	test' = do name <- funcName
  	           e <- expr
  	           return (Function name e)

factor = factor' <|> unit 
  where 
    factor' = do  char '('
                  e <- expr
                  char ')'
                  return e

unit = (do n <- number 
           return (Num n))
	   <|>
	   (do v <- (char 'x') 
	       return X )

