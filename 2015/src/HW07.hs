{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------
-- | liftM
--
-- >>> liftM (+1) (Just 5)
-- Just 6
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= \a -> return $ f a

-- | swapV
--
-- >>> swapV 0 2 (V.fromList [1, 2, 3])
-- Just [3,2,1]
--
-- >>> swapV 0 2 (V.fromList [1, 2])
-- Nothing
swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = liftM2 (\vi vj -> v // [(i, vj), (j, vi)]) (v !? i) (v !? j)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM mf (x:xs) = liftM2 (:) (mf x) (mapM mf xs)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v !?) <$> getRandomR (1, V.length v)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec i = V.fromList <$> replicateM i getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR i (x, y) = V.fromList <$> replicateM i (getRandomR (x, y))

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldr swap v <$> mapM getRandomSwap [n-1,n-2..1]
  where
    n = V.length v
    getRandomSwap i = (\j -> (i, j)) <$> getRandomR (0, i)
    swap (i, j) v' = v' // [(i, v' ! j), (j, v' ! i)]

-- Exercise 6 -----------------------------------------
-- | Partition a Vector around the element at a given index
--
-- >>> partitionAt (V.fromList [5,2,8,3,6,1]) 3
-- ([2,1],3,[5,8,6])
--
-- >>> partitionAt (V.fromList [1,6,4,7,2,4]) 2
-- ([1,2],4,[6,7,4])
partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (before, pivot, after)
    where pivot = v ! i
          v' = V.take i v V.++ V.drop (i+1) v
          before = V.filter (<pivot) v'
          after = V.filter (>=pivot) v'

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

-- | qsort
--
-- >>> qsort (V.fromList [5,2,8,3,6,1])
-- [1,2,3,5,6,8]
qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.null v = V.empty
  | otherwise = qsort [ y | y <- xs, y < x ]
                <> (x `cons` qsort [ y | y <- xs, y >= x ])
  where x = V.head v
        xs = V.tail v

-- Exercise 8 -----------------------------------------
-- | qsortR
--
-- >>> evalRandIO $ qsortR (V.fromList [5,2,8,3,6,1])
-- [1,2,3,5,6,8]
qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = fmap (partitionAt v) rip >>= mergeSorted
  where rip = getRandomR (0, V.length v - 1)
        mergeSorted (a, p, b) = liftM2 (<>) (qsortR a) pb'
          where pb' = fmap (p `cons`) (qsortR b)


-- Exercise 9 -----------------------------------------

-- | Selection
--
-- >>> evalRandIO $ select 3 (V.fromList [5,2,8,3,6,1])
-- Just 5
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | V.null v  = return Nothing
  | otherwise = fmap (partitionAt v) rip >>= findI
  where rip = getRandomR (0, V.length v - 1)
        findI (a, p, b)
          | i < na = select i a
          | i > na = select (i-na-1) b
          | otherwise = return $ Just p
          where na = V.length a

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card l s | l <- labels, s <- suits ]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.null d = Nothing
  | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n = go []
  where go cards deck
          | length cards >= n = return (cards, deck)
          | otherwise = do
                        (card, deck') <- nextCard deck
                        go (cards ++ [card]) deck'

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
