module Lab1 where

import Test.QuickCheck
-- ============================= INTRODUCTION ==============================
-- In this lab assignment, we will implement the power function in two
-- different ways.

-- One possible implementation of this function is :
power :: Num a => a -> Integer -> a
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


-- =========================================================================





-- ================================ PART 1 =================================

-- We want to know how many "steps" are being used by the function power n k
-- considering that n and k are known

-- This function is recursive and each call takes 1 step.
-- power n k calls power n (k-1) which calls power n (k-2) etc until power n 0
-- Consequently, k steps will be used by this function

-- =========================================================================



-- ================================ PART 2 =================================

-- Function that will initialise a list of k elements all being equal to n
-- We used list comprehension :
initialiseList :: a -> Integer -> [a]
initialiseList n k | k<0 = error "power: negative argument"
                   | otherwise= [n | x<-[1..k]]

-- Calculate n^k multuplying all the terms of a list of k elements all being equal to n
power1:: Num a => a -> Integer -> a
power1 n k | k < 0 = error "initialiseList : k must be positive"
power1 n k = product (initialiseList n k)

-- =========================================================================

-- ================================ PART 3 =================================

-- Calculate n^k, if k even it returns (n*n)^(k/2) and if it is odd, n*(n)^(k-10)
power2:: Num a => a -> Integer -> a
power2 n 0 = 1
power2 n k | k < 0 = error "initialiseList : k must be positive"
           | even k = power2 (n*n) (div k 2)
           | odd k = n*power2 n (k-1)


-- =========================================================================

-- ================================ PART 4 =================================

-- -------------------------------- TASK A ---------------------------------
{-
Test cases:
1. n and k positive integers -> basics tests to see if it is working as it
   should be.
2. n = 0  -> check if power1 retrieves 1 or nothing
3. n not Integer, k Integer -> power will have problems
4. odd value of k and negative n -> result should be negative
5. even value of k  and negative n -> result should be positive
6. k=0 -> result should be 1 for any numeric value of n

We tried also making k not integer, but that will only result in an error for
all the power functions and not make the prop_powers false same for char and strings.

-}
-- -------------------------------------------------------------------------

-- -------------------------------- TASK B ---------------------------------
{-
  This function tests if for the same input values n and k all three
  power functions return the same output
-}

prop_powers n k = power n k == power1 n k && power2 n k == power1 n k

-- -------------------------------------------------------------------------

-- -------------------------------- TASK C ---------------------------------
{-
  We put examples for the above test cases in an array and performed
  prop_powers on each of them. The results were combined with the and function.
-}

listOfCases = [(5.0,2),(0.0,5),(2.0,2),(2.5,6),((-2.0),3),((-2.0),8),(5.0,0)]

doingTests listOfPairs = and [prop_powers (fst x) (snd x) | x<-listOfPairs ]

-- -------------------------------------------------------------------------

-- -------------------------------- TASK D ---------------------------------
{-
  The Problem in QuickCheck occured while using negative values for k. If we
  take the absolute value of k, we can avoid these cases and QuickCheck will
  succeed.
-}

prop_powers' n k = prop_powers n (abs k)

-- -------------------------------------------------------------------------
-- =========================================================================
