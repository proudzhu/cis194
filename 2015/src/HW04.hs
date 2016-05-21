{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------
-- | Implement the (==) function
--
-- >>> P [1,2,3] == P [1,2,3]
-- True
--
-- >>> P [1,2] /= P [1,2,3]
-- True
instance (Num a, Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = p1 == p2

-- Exercise 3 -----------------------------------------
-- | Implement the (Show) function
--
-- >>> show (P [1,0,0,2])
-- "2x^3 + 1"
--
-- >>> show (P [0,-1,2])
-- "2x^2 + -x"
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P arr) = displayArr 0 arr
        where
            displayArr _ [] = ""
            displayArr len [k] = displayTerm len k
            displayArr len (k:ks)
                | k == 0 = displayArr (len + 1) ks
                | otherwise = displayArr (len + 1) ks ++ " + " ++ displayTerm len k
            displayTerm _ 0 = ""
            displayTerm 0 coef = show coef
            displayTerm 1 1 = "x"
            displayTerm 1 (-1) = "-x"
            displayTerm 1 coef = show coef ++ "x"
            displayTerm pow 1 = "x^" ++ show pow
            displayTerm pow coef = show coef ++ "x^" ++ show pow

-- Exercise 4 -----------------------------------------
-- | Implement the (+) function
--
-- >>> P [5,0,1] + P [1,1,2]
-- 3x^2 + x + 6
--
-- >>> P [1,0,1] + P [1,1]
-- x^2 + x + 2
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P arr1) (P arr2)
    | len1 >= len2 = P (zipWith (+) arr1 (arr2 ++ replicate (len1 - len2) 0))
    | otherwise = plus (P arr2) (P arr1)
    where len1 = length arr1
          len2 = length arr2


-- Exercise 5 -----------------------------------------
-- | Implement the (*) function
--
-- >>> P [1,1,1] * P [2,2]
-- 2x^3 + 4x^2 + 4x + 2
times :: Num a => Poly a -> Poly a -> Poly a
times (P arr1) (P arr2) = foldr (+) (P [0]) $ multiplyArr arr1 arr2
    where multiplyArr [] _ = []
          multiplyArr (k:ks) arr = P (map (* k) arr) : multiplyArr ks (0 : arr)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = (* P [-1])
    fromInteger k = P [fromInteger k]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------
-- | Evalute
--
-- >>> applyP (x^2 + 2*x + 1) 1
-- 4
--
-- >>> applyP (x^2 + 2*x + 1) 2
-- 9
applyP :: Num a => Poly a -> a -> a
applyP (P arr) = evaluateCoefs 0 arr
    where
        evaluateCoefs _ [] _ = 0
        evaluateCoefs acc [k] val = k * (val ^ acc)
        evaluateCoefs acc (k:ks) val = k * (val ^ acc) + evaluateCoefs (acc + 1) ks val

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f = deriv (nderiv (n - 1) f)

-- Exercise 9 -----------------------------------------
-- | Deriv
--
-- >>> deriv (x^2 + 3*x + 5)
-- 2x + 3
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P [0]
    deriv (P arr) = P $ tail (derivCoefs 0 arr)
        where
        derivCoefs _ [] = []
        derivCoefs pow (k:ks) = (k * pow) : derivCoefs (pow + 1) ks
