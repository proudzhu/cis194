module Hw3 where

-- | Output a list of lists
-- The first list should be the same as the input list
-- The second list should contain every second element from the input list
-- ...
-- The nth list should contain evary nth element form the input list
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
--
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
--
-- >>> skips [1]
-- [[1]]
--
-- >>> skips [True,False]
-- [[True,False],[False]]
--
-- >>> skips []
-- []
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (everyNthElement xs) [1..(length xs)]

everyNthElement :: [a] -> Int -> [a]
everyNthElement xs n = case drop (n-1) xs of
                         (y:ys) -> y : everyNthElement ys n
                         []     -> []


-- | A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
--
-- >>> localMaxima [2,3,4,1,5]
-- [4]
--
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima = reverse . lmax

lmax :: [Integer] -> [Integer]
lmax [] = []
lmax [_] = []
lmax (a:b:xs) = third $ foldl f (a,b,[]) xs
  where
    third (_, _, x) = x
    f (x, y, ls) z = (y, z, if y > x && y > z then y:ls else ls)

-- | Histogram
--
-- >> histogram [1,1,1,5]
