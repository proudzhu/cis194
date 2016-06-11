{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

import Data.Bits (xor)
import Data.Word (Word8)

-- Exercise 1 -----------------------------------------
-- | Get encryption key
--
-- >>> getSecret "src/clues/dog.jpg" "src/clues/dog-original.jpg"
-- "Haskell Is Great!"
getSecret :: FilePath -> FilePath -> IO ByteString
getSecret p1 p2 = do
    image1 <- BS.readFile p1
    image2 <- BS.readFile p2
    let l1 = BS.unpack image1
    let l2 = BS.unpack image2
    let secrets = filter ((/= 0) . toInteger) $ zipWith xor l1 l2
    return $ BS.pack secrets

-- Exercise 2 -----------------------------------------
decrypt :: [Word8] -> [Word8] -> [Word8]
decrypt _ [] = []
decrypt key enc = zipWith xor key enc ++ decrypt key (drop (length key) enc)

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
    let pathenc = path ++ ".enc"
    encfile <- BS.readFile pathenc
    let decrypted = decrypt (BS.unpack key) (BS.unpack encfile)
    BS.writeFile path (BS.pack decrypted)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    encdata <- BS.readFile file
    return $ decode encdata

-- Exercise 4 -----------------------------------------

findBadTs :: Foldable t => Maybe (t TId) -> Maybe [Transaction] -> Maybe [Transaction]
findBadTs (Just victims) (Just transactions) = Just $ filter (\transaction -> tid transaction `elem` victims) transactions
findBadTs Nothing (Just tran) = Just tran
findBadTs _ Nothing = Nothing

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs file1 file2 = do
    victList <- parseFile file1 :: IO (Maybe [TId])
    tranList <- parseFile file2 :: IO (Maybe [Transaction])
    return $ findBadTs victList tranList


-- Exercise 5 -----------------------------------------
-- | updateMap
--
-- >>> :{
-- let t = Transaction { from   = "a"
--                      , to     = "b"
--                      , amount = 10
--                      , tid    = "atob" }
-- in updateMap t Map.empty
-- :}
-- fromList [("a",-10),("b",10)]
updateMap :: Transaction -> Map String Integer -> Map String Integer
updateMap t = Map.insertWith (+) (to t) (amount t) . Map.insertWith (+) (from t) (-(amount t))

-- | getFlow
--
-- >>> :{
-- let ts = [ Transaction "a" "b" 10 "a2b"
--          , Transaction "b" "c" 7  "b2c"
--          , Transaction "c" "a" 5  "c2a"]
-- in getFlow ts
-- :}
-- fromList [("a",-5),("b",3),("c",2)]
getFlow :: [Transaction] -> Map String Integer
getFlow ts = foldr updateMap Map.empty ts

-- Exercise 6 -----------------------------------------
getMaxFromMap :: Map String Integer -> [String]
getMaxFromMap m = go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

getCriminal :: Map String Integer -> String
getCriminal m = head $ getMaxFromMap m

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

