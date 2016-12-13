module Expr where

import Parsing
import Data.Maybe
import Data.Char

data Expr = Num Double | Add Expr Expr | Mul Expr Expr | X | Sin Expr | Cos Expr
 deriving (Eq,Show)


type Name = String

-- Function that converts any expression to string
{-
   Parentheses are required only in the following cases:
		1- When the arguments of a *-expression use +. For example: (3.1+4.2)*7
		2- When the argument of sin or cos uses * or +. For example: sin (3.2*x)
-}
showExpr :: Expr -> String
showExpr (Num f) | f<0 = "("++show f++")"
                 | otherwise = show f
showExpr X = "x"
showExpr (Sin e) = "sin "++ showFactorSin e
showExpr (Cos e) = "cos "++ showFactorSin e
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactorMul e1 ++ "*" ++ showFactorMul e2

-- Helper function that add parenthesis for case 1 above
showFactorSin::Expr -> String
showFactorSin (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showFactorSin (Mul e1 e2) = "(" ++ showFactorMul e1 ++ "*" ++ showFactorMul e2 ++")"
showFactorSin e = showExpr e

-- Helper function that add parenthesis for case 2 above
showFactorMul:: Expr -> String
showFactorMul (Add e1 e2) =  "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showFactorMul e = showExpr e

-- Function that, given an expression, and the value for the variable x, calculates the value of the expression
eval :: Expr -> Double -> Double
eval X valueVar = valueVar
eval (Num n) valueVar = n
eval (Sin e) valueVar = sin (eval e valueVar)
eval (Cos e) valueVar = cos (eval e valueVar)
eval (Add e1 e2) valueVar = (eval e1 valueVar) + (eval e2 valueVar)
eval (Mul e1 e2) valueVar = (eval e1 valueVar) * (eval e2 valueVar)

-- Function that, given a string, tries to interpret the string as an expression,
-- and returns Just of that expression if it succeeds.
-- Otherwise, Nothing will be returned. If the string is not completely parsed
--as an expression we return Nothing.

readExpr :: String -> Maybe Expr
readExpr s | (parsingRes == Nothing) = Nothing
           | (stringLeft /= "") = Nothing
           | otherwise = mExp
  where
     parsingRes = parse expr s
     tmp = fromJust(parsingRes)
     stringLeft = snd(tmp)
     mExp = Just (fst(tmp))

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
number = readsP


-- | Parse a name of a function
funcName :: Parser String
funcName = do s <- oneOrMore (sat isLower)
              char ' '
              return (s)


expr, term, factor :: Parser Expr

expr = expr' <|> term
  where
    expr' = do t <- term
               char ' '
               char '+'
               char ' '
               e <- expr
               return (Add t e)

term = term' <|> factor
  where
    term' = do f <- factor
               char '*'
               t <- term
               return (Mul f t)

factor = ((factor' <|> func) <|> num)  <|> var
  where
    factor' = do  char '('
                  e <- expr
                  char ')'
                  return e

    -- Parse a function
    func = (func1 <|> func2) <|> func3

    -- Case sin 2 or sin x
    func1 = do name <- cosinus <|> sinus
               n <- num <|> var
               return (name n)

    -- Case sin cos ....
    func2 = do name <- cosinus <|> sinus
               f <- func
               return (name f)

    -- Case sin (...)
    func3 = do name <- cosinus <|> sinus
               char '('
               e <- expr
               char ')'
               return (name e)

    cosinus = do char 'c'
                 char 'o'
                 char 's'
                 char ' '
                 return (Cos)

    sinus = do char 's'
               char 'i'
               char 'n'
               char ' '
               return (Sin)      

    num = do n <- number
             return (Num n)

    var = do v <- (char 'x')
             return X


-- Function that simplifies expressions so that :
--      - subexpressions not involving variables are always simplified to their smallest representation
--      - (sub)expressions representing x + 0 , 0 * x and 1 * x and similar terms are always simplified
--      - sums of multiples of x are combined to one multiple

simplify :: Expr -> Expr
simplify e | hasVariable e == False = (Num (eval e 0))
           | otherwise = simplify' e
  where
    simplify' :: Expr -> Expr
    simplify' (Add X X) = Mul (Num 2) X
    simplify' (Add (Mul (Num m) X) X) = Mul (Num(m+1)) X
    simplify' (Add X (Mul (Num m) X)) = Mul (Num(m+1)) X
    simplify' (Add (Mul (Num m) X) (Mul (Num n) X)) = Mul (Num (m+n)) X
    simplify' (Add e1 e2) | e1 == (Num 0) = e2
                          | e2 == (Num 0) = e1
                          | otherwise = (Add (simplify e1) (simplify e2))
    simplify' (Mul e1 e2) | e1 == (Num 0) || e2 == (Num 0) = (Num 0)
                          | e1 == (Num 1) = e2
                          | e2 == (Num 1) = e1
                          | otherwise = (Mul (simplify e1) (simplify e2))
    simplify' (Cos e) = (Cos (simplify e))
    simplify' (Sin e) = (Sin (simplify e))
    simplify' (Num n) = (Num n)
    simplify' X = X
    -- Helper function that searches if an expression contains a variable
    hasVariable :: Expr -> Bool
    hasVariable (Num n) = False
    hasVariable X = True
    hasVariable (Sin e) = hasVariable e
    hasVariable (Cos e) = hasVariable e
    hasVariable (Add e1 e2) = hasVariable e1 || hasVariable e2
    hasVariable (Mul e1 e2) = hasVariable e1 || hasVariable e2


-- Function that differentiates the expression (with respect to x).
differentiate :: Expr -> Expr
differentiate e = simplify (differentiate' e)
  where
        differentiate' :: Expr -> Expr
        differentiate' (Num n) = (Num 0)
        differentiate' (X) = (Num 1)
        differentiate' (Add e1 e2) = Add (differentiate e1) (differentiate e2)
        differentiate' (Mul e1 e2) = Add (Mul (differentiate e1) e2) (Mul e1 (differentiate e2))
        differentiate' (Sin e) = Mul (differentiate e) (Cos e)
        differentiate' (Cos e) = Mul  (Num(-1)) (Mul (differentiate e) (Sin e))
