module ExprQC where

import Test.QuickCheck
import Expr
import Data.Maybe

-- Property that says that first showing and then reading an expression
-- (using your functions showExpr and readExpr) should produce "the same"
--  result as the expression we started with.

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = almostEqual (eval e 0) (eval exp 0)
	where
		 almostEqual :: Double -> Double -> Bool
		 almostEqual x y = abs (x-y) < 0.001
		 exp = fromJust (readExpr (showExpr e))


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
