{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- | Get the last digit from a number
--
-- >>> lastDigit 123
-- 3
--
-- >>> lastDigit 0
-- 0
lastDigit :: Integer -> Integer
lastDigit i = i `mod` 10

-- | Drop the last digit from a number
--
-- >>> dropLastDigit 123
-- 12
--
-- >>> dropLastDigit 5
-- 0
dropLastDigit :: Integer -> Integer
dropLastDigit i = i `div` 10

-- Exercise 2 -----------------------------------------
-- | Break a number into a list of its digits in reverse order
--
-- >>> toRevDigits 1234
-- [4,3,2,1]
--
-- >>> toRevDigits 0
-- []
--
-- >>> toRevDigits (-17)
-- []
toRevDigits :: Integer -> [Integer]
toRevDigits i
    | i <= 0 = []
    | otherwise = lastDigit i : toRevDigits (dropLastDigit i)

-- | Break a number into a list of its digits.
--
-- >>> toDigits 1234
-- [1,2,3,4]
--
-- >>> toDigits 0
-- []
--
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits i = reverse (toRevDigits i)

-- Exercise 3 -----------------------------------------

-- | Double every second number in a list starting on the left.
--
-- >>> doubleEveryOther [4,9,5,5]
-- [4,18,5,10]
--
-- >>> doubleEveryOther [0,0]
-- [0,0]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x1:x2:xs) = x1 : x2 * 2 : doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- | Calculate the sum of all the digits in every Integer.
--
-- >>> sumDigits [10,5,18,4]
-- 19
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = foldr ((+) . sum . toDigits) 0 x


-- Exercise 5 -----------------------------------------

-- | Validate a credit card number using the above functions.
--
-- >>> luhn 5594589764218858
-- True
--
-- >>> luhn 1234567898765432
-- False
luhn :: Integer -> Bool
luhn i = sumDigits (doubleEveryOther (toRevDigits i)) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

-- | hanoi
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++
                [(a, b)] ++
                hanoi (n-1) c b a

isqrt :: Integer -> Integer
isqrt = round . (sqrt :: Double -> Double) . fromIntegral

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = hanoi 2 a b c
hanoi4 n a b c d =
    hanoi4 k a c b d ++
    hanoi (n-k) a b d ++
    hanoi4 k c b a d
    where
      k = 4 - isqrt (2 * n + 1) + 1

hanoiR :: Integer -> [Peg] -> [Move]
hanoiR 0 _ = []
hanoiR 1 (p1 : p2 : _) = [(p1, p2)]
hanoiR n (p1 : p2 : p3 : rest) =
    hanoiR k (p1 : p3 : p2 : rest) ++
    hanoiR (n - k) (p1 : p2 : rest) ++
    hanoiR k (p3 : p2 : p1 : rest)
    where
      k
        | null rest = n - 1
        | otherwise = n - isqrt (2 * n + 1) + 1
