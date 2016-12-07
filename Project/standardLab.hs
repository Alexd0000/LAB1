module StandardLab where

import Test.QuickCheck
import Parsing
import Data.Maybe
import Data.Char

data Expr = Num Double | Add Expr Expr | Mul Expr Expr | X | Function Name Expr
 deriving (Eq)

type Name = String

exExpr1 = Num 7											-- 7

exExpr2 = Add (Add X (Mul (Num 2) (Num 5))) (Num 2.7)   --  x + 2*5 + 2.7

exExpr3 = Mul (Num 2) (Function "sin" (Add (Num 2.3) X))				-- 2*sin(2.3+x)

exExpr4 = Mul (Add (Num 3) X) (Add (Num 2) (Num 2.5)) 	-- (3+x)*(2+2.5)

exExpr5 = Mul (Add (Mul (Num 2) (Function "sin" (Add (Num 2.3) X))) (Num 9.1)) (Add (Num 2.3) X) -- (2*sin(2.3+x)+9.1)*(2.3+x)

exExpr6 = Function "sin" (Function "cos" X)   -- sin cos x

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
showFactorSin e = showExpr e

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
-- Otherwise, Nothing will be returned. If the string is not completely parsed as an expression we return Nothing.

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
number = numberWithDot <|> numberWithoutDot


-- | Parse a name of a function
funcName :: Parser String
funcName = do s <- oneOrMore (sat isLower)
              char ' '
              return (s)


leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)


expr, term, factor :: Parser Expr

expr = expr' <|> term
  where
    expr' = do t <- term
               char '+'
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
    func = do name <- funcName
              e <-expr
              return (Function name e)

    num = do n <- number 
             return (Num n)

    var = do v <- (char 'x')
             return X

-- Property that says that first showing and then reading an expression 
-- (using your functions showExpr and readExpr) should produce "the same"
--  result as the expression we started with.
--prop_ShowReadExpr :: Expr -> Bool
--prop_ShowReadExpr e = e== (fromJust (readExpr (showExpr e)))


-- Defining a generator for expressions
arbExpr :: Int -> Gen Expr
arbExpr size = frequency[(1,rNum),(size,rOp),(1,rVar),(size,rFunc)]
  where
  	rNum = do r <- arbitrary::Gen Double
  	          return (Num r)

  	rVar  = elements [X]

  	rFunc = do name <- elements ["sin","cos"]
  	           e <- arbExpr (size-1)
  	           return (Function name e)

  	rOp = do op <- elements [Add,Mul]
  	         let size'= size `div` 2
  	         e1 <- arbExpr size'
  	         e2 <- arbExpr size'
  	         return (op e1 e2)

instance Arbitrary Expr where
  arbitrary = sized arbExpr



-- Function that simplifies expressions so that :
--      - subexpressions not involving variables are always simplified to their smallest representation
--      - (sub)expressions representing x + 0 , 0 * x and 1 * x and similar terms are always simplified

--simplify :: Expr -> Expr
--simplify e | (not (hasVariable e) == True) = (Num (eval e 0))
--           | otherwise




simplify :: Expr -> Expr
simplify (Add e1 e2) | ((e1 == X) && e2 == (Num 0)) || ((e2 == X) && e1 == (Num 0)) = X
                     | otherwise = (Add (simplify e1) (simplify e2))
simplify (Mul e1 e2) | ((e1 == X) && e2 == (Num 0)) || ((e2 == X) && e1 == (Num 0)) = (Num 0)
                     | ((e1 == X) && e2 == (Num 1)) || ((e2 == X) && e1 == (Num 1)) = X
                     | otherwise = (Mul (simplify e1) (simplify e2))
simplify (Function name e) = (Function name (simplify e))
simplify (Num n) = (Num n)
simplify X = X

--simplifyWithoutVariable :: Expr -> Expr
--simplifyWithoutVariable e | (not (hasVariable e) == True) = (Num (eval e 0))
--                          | otherwise = simplify e               

-- Helper function that search if an expression contain a variable
hasVariable :: Expr -> Bool
hasVariable (Num n) = False
hasVariable X = True
hasVariable (Function name e) = hasVariable e
hasVariable (Add e1 e2) = hasVariable e1 || hasVariable e2
hasVariable (Mul e1 e2) = hasVariable e1 || hasVariable e2
