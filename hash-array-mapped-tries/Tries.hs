module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

-- Counts number of 1 bits in a binary number
countOnes :: Int -> Int
countOnes 0 = 0
countOnes 1 = 1
countOnes n
 = n `mod` 2 + countOnes (n `div` 2)

-- Counts 1 bits in a binary number from a given index (up to 16 bit strings)
countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
 = (length . filter (== '1')) (showBitVector filteredBits i)
 where
   filteredBits = (bit i - 1) .&. n

-- Extracts a block of bits from an integer given the block size and index
getIndex :: Int -> Int -> Int -> Int
getIndex n block bsize
  = undefined
  
-- Replaces the nth item of a list with a value
-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (x : xs) n = n : xs
replace i (x : xs) n
 = x : replace (i - 1) xs n

-- Inserts an item at the nth position in a list
-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 n xs = n : xs
insertAt i n (x : xs)
 = x : insertAt (i - 1) n xs

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie
  = undefined

{-
--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)
-}

member :: Int -> Hash -> Trie -> Int -> Bool
member
  = undefined

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert
  = undefined

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie
  = undefined
