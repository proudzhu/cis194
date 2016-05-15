{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- | A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- | A code is defined to simply be a list of Pegs
type Code = [Peg]

-- | A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- | List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- | Get the number of exact matches between the actual code and the guess
--
-- >>> exactMatches [Red,Blue,Green,Yellow] [Blue,Green,Yellow,Red]
-- 0
--
-- >>> exactMatches [Red,Blue,Green,Yellow] [Red,Purple,Green,Orange]
-- 2
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length (filter (uncurry (==)) (zip c1 c2))

-- Exercise 2 -----------------------------------------

-- | For each peg in xs, count how many times is occurs in ys
--
-- >>> countColors [Red,Blue,Yellow,Purple]
-- [1,0,1,1,0,1]
--
-- >>> countColors [Green,Blue,Green,Orange]
-- [0,2,1,0,1,0]
countColors :: Code -> [Int]
countColors cs = map (`countColor` cs) colors
  where countColor c = foldl (\x y -> if c == y then 1 + x else x) 0

-- | Count number of matches between the actual code and the guess
--
-- >>> matches [Red,Blue,Yellow,Orange] [Red,Orange,Orange,Blue]
-- 3
matches :: Code -> Code -> Int
matches c1 c2 = sum (zipWith min (countColors c1) (countColors c2))

-- Exercise 3 -----------------------------------------

-- | Construct a Move from a guess given the actual code
--
-- >>> getMove [Red,Blue,Yellow,Orange] [Red,Orange,Orange,Blue]
-- Move [Red,Orange,Orange,Blue] 1 2
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exMatchNum (matchNum - exMatchNum)
  where matchNum = matches secret guess
        exMatchNum = exactMatches secret guess

-- Exercise 4 -----------------------------------------
-- | isConsistent
--
-- >>> isConsistent (Move [Red,Red,Blue,Green] 1 1) [Red,Blue,Yellow,Purple]
-- True
--
-- >>> isConsistent (Move [Red,Red,Blue,Green] 1 1) [Red,Blue,Red,Purple]
-- False
isConsistent :: Move -> Code -> Bool
isConsistent (Move guess i j) c = getMove guess c == Move c i j

-- Exercise 5 -----------------------------------------
-- | filterCodes
filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------
-- | allCodes
allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concatMap nextStep (allCodes (n - 1))
  where nextStep code = map (:code) colors

-- Exercise 7 -----------------------------------------
-- | solve
solve :: Code -> [Move]
solve c = choose c $ allCodes (length c)

choose :: Code -> [Code] -> [Move]
choose _ [] = []
choose c (x:xs) =
    if x == c then [m] else m : choose c (filterCodes m xs)
        where m = getMove c x

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
