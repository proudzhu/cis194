{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------
-- | fib
--
-- >>> fib 0
-- 1
--
-- >>> fib 1
-- 1
--
-- >>> fib 2
-- 2
--
-- >>> fib 9
-- 55
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib i = fib (i - 1) + fib (i - 2)

-- | fibs1
--
-- >>> take 10 fibs1
-- [1,1,2,3,5,8,13,21,34,55]
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------
-- | fibs2
--
-- >>> take 10 fibs2
-- [1,1,2,3,5,8,13,21,34,55]
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------
-- | sRepeat
--
-- >>> sTake 10 $ sRepeat 0
-- [0,0,0,0,0,0,0,0,0,0]
sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

-- | sIterate
--
-- >>> sTake 5 $ sIterate ('x':) "o"
-- ["o","xo","xxo","xxxo","xxxxo"]
sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

-- | sInterleave
--
-- >>> sTake 10 $ sInterleave (sRepeat 0) (sRepeat 1)
-- [0,1,0,1,0,1,0,1,0,1]
sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = Cons x (sInterleave y xs)

-- | sTake
--
-- >>> sTake 5 $ sRepeat 0
-- [0,0,0,0,0]
sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake 1 (Cons x _) = [x]
sTake n (Cons x xs) = x : sTake (n - 1) xs

-- Exercise 6 -----------------------------------------
-- | nats
--
-- >>> sTake 10 nats
-- [0,1,2,3,4,5,6,7,8,9]
nats :: Stream Integer
nats = sIterate (+1) 0

-- | ruler
--
-- >>> sTake 10 ruler
-- [0,1,0,2,0,1,0,3,0,1]
ruler :: Stream Integer
ruler = go 0
  where go n = sInterleave (sRepeat n) (go (n + 1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand r0 = Cons r1 (rand r1)
  where r1 = (1103515245 * r0 + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 221 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = go x x xs
  where
    go minX maxX [] = Just (minX, maxX)
    go minX maxX (y:ys)
      | y < minX = go y maxX ys
      | y > maxX = go minX y ys
      | otherwise = go minX maxX ys

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
--
-- >>> fastFib 9
-- 55
fastFib :: Int -> Integer
fastFib i = fastFibHelper $ (Matrix 1 1 1 0) ^ (i + 1)

fastFibHelper :: Matrix -> Integer
fastFibHelper (Matrix _ _ x _) = x

data Matrix = Matrix Integer Integer Integer Integer
              deriving (Eq)

instance Num Matrix where
  (*) (Matrix x00 x01 x10 x11) (Matrix y00 y01 y10 y11)
      = (Matrix (x00 * y00 + x01 * y10) (x00 * y01 + x01 * y11)
                (x10 * y00 + x11 * y10) (x10 * y01 + x11 * y11))
  (+) = undefined
  fromInteger = undefined
  negate = undefined
  abs = undefined
  signum = undefined

