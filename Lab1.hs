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
                   | k==0 = []
                   | k>0 = [n | x<-[1..k]]

-- Calculate n^k multuplying all the terms of a list of k elements all being equal to n
-- power1:: Num a => a -> Integer -> [a]
power1 n k | k < 0 = error "initialiseList : k must be positive"
power1 n k = product (initialiseList n k)

-- =========================================================================

-- ================================ PART 3 =================================

-- Calculate n^k, if k even it returns (n*n)^(k/2) and if it is odd, n*(n)^(k-10)
power2 n k | k < 0 = error "initialiseList : k must be positive" 
           | k == 0 = 1 
           | even k = power2 (n*n) (div k 2) 
           | odd k = n*power2 n (k-1)


-- =========================================================================

-- ================================ PART 4 =================================

-- -------------------------------- TASK A ---------------------------------
-- We need to come up with a number of test cases

-- 1 : n=5 and k=0
-- -------------------------------------------------------------------------

-- -------------------------------- TASK B ---------------------------------

prop_powers n k = power n k == power1 n k && power2 n k == power1 n k

-- -------------------------------------------------------------------------

-- -------------------------------- TASK C ---------------------------------

-- -------------------------------------------------------------------------

-- -------------------------------- TASK D ---------------------------------

-- -------------------------------------------------------------------------
-- =========================================================================