module Expr where

import Parsing
import Data.Maybe
import Data.Char
import Test.QuickCheck

data Expr = Num Double | Add Expr Expr | Mul Expr Expr | X | Sin Expr | Cos Expr
 deriving (Eq,Show)

-- Smart Constructor to be used always instead of Add
smartAdd :: Expr -> Expr -> Expr
smartAdd (Num a) (Num b)                 = Num (a+b)
smartAdd (Num 0) b                       = b
smartAdd a (Num 0)                       = a
smartAdd X X                             = smartMul (Num 2) X
smartAdd (Mul (Num m) X) X               = smartMul (Num (m+1)) X
smartAdd X (Mul (Num m) X)               = smartMul (Num (m+1)) X
smartAdd (Mul (Num m) X) (Mul (Num n) X) = smartMul (Num (m+n)) X
smartAdd a b                             = Add a b

-- Smart Constructor to be used always instead of Mul
smartMul :: Expr -> Expr -> Expr
smartMul (Num a) (Num b)                = Num (a*b)
smartMul a b | a == Num 0 || b == Num 0 = Num 0
             | a == Num 1               = b
             | b == Num 1               = a
smartMul (Num a) (Mul (Num b) e)        = Mul (Num(a*b)) e
smartMul a b                            = Mul a b



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
showExpr (Sin e) = "sin "++ showFactorFunc e
showExpr (Cos e) = "cos "++ showFactorFunc e
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactorMul e1 ++ "*" ++ showFactorMul e2

-- Helper function that add parenthesis for case 1 above
showFactorFunc::Expr -> String
showFactorFunc (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++")"
showFactorFunc (Mul e1 e2) = "(" ++ showFactorMul e1 ++ "*" ++ showFactorMul e2 ++")"
showFactorFunc e = showExpr e

-- Helper function that add parenthesis for case 2 above
showFactorMul:: Expr -> String
showFactorMul (Add e1 e2) =  "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++")"
showFactorMul e = showExpr e

-- Function that, given an expression, and the value for the variable x,
--calculates the value of the expression
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
               zeroOrMore (char ' ')
               char '+'
               zeroOrMore (char ' ')
               e <- expr
               return (Add t e)

term = term' <|> factor
  where
    term' = do f <- factor
               zeroOrMore (char ' ')
               char '*'
               zeroOrMore (char ' ')
               t <- term
               return (Mul f t)

factor = ((factor' <|> func) <|> num)  <|> var
  where
    factor' = do  char '('
                  e <- expr
                  char ')'
                  return e

    -- Parse a function
    func = do name <- cosinus <|> sinus
              fac <- factor
              return (name fac)

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

--simplifies Expressions using the smart constructors
simplify :: Expr -> Expr
simplify (Add e1 e2) = smartAdd (simplify e1) (simplify e2)
simplify (Mul e1 e2) = smartMul (simplify e1) (simplify e2)
simplify (Cos e) = Cos (simplify e)
simplify (Sin e) = Sin (simplify e)
simplify (Num n) = Num n
simplify X = X

--property to test simplify
prop_simplify :: Expr -> Double -> Property
prop_simplify e x = collect e (eval (simplify e) x ~== eval e x)

--Equality operator accepting little deviations
(~==) :: (Ord a, Fractional a) => a -> a -> Bool
x ~== y = x-y < 1e-10

-- Function that differentiates the expression (with respect to x).
differentiate :: Expr -> Expr
differentiate e = differentiate' e
  where
        differentiate' :: Expr -> Expr
        differentiate' (Num n) = (Num 0)
        differentiate' (X) = (Num 1)
        differentiate' (Add e1 e2) = smartAdd (differentiate e1) (differentiate e2)
        differentiate' (Mul e1 e2) = smartAdd (smartMul (differentiate e1) e2) (smartMul e1 (differentiate e2))
        differentiate' (Sin e) = smartMul (differentiate e) (Cos e)
        differentiate' (Cos e) = smartMul  (Num(-1)) (smartMul (differentiate e) (Sin e))
