module Hw1 where

-- | Convert positive Intergers to a list of digits
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
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- | Convert positive Intergers to a list of reversed digits
--
-- >>> toDigitsRev 1234
-- [4,3,2,1]
--
-- >>> toDigitsRev 0
-- []
--
-- >>> toDigitsRev (-17)
-- []
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- | Double every other number beginning from the left
--
-- >>> doubleEveryOther' [8,7,6,5]
-- [8,14,6,10]
--
-- >>> doubleEveryOther' [1,2,3]
-- [1,4,3]
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:xs) = x : y * 2 : doubleEveryOther' xs

-- | Double every other number beginning from the right
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
--
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOther' (reverse xs))

-- | Calculate the sum of all digits
--
-- >>> sumDigits [16,7,12,5]
-- 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- | Indicates whether an Integer could be a valid credit card number
--
-- >>> validate 4012888888881881
-- True
--
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)

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
