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
  = blockshifted .&. (bit bsize - 1)
  where
    blockpos = block * bsize
    blockshifted = shiftR n blockpos
  
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
sumTrie _ _ (Node 0 []) = 0
sumTrie f g (Node _ [Term x])
  = f x
sumTrie f g (Leaf xs)
 = g xs
sumTrie f g (Node _ [SubTrie tr])
 = sumTrie f g tr
sumTrie f g (Node n (x : xs))
 = sumTrie f g (Node n [x]) + sumTrie f g (Node n xs)

-- Computes size of trie
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

-- Computes number of bins
binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

-- Computes average number of values in each bin
meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

-- todo finish
member :: Int -> Hash -> Trie -> Int -> Bool
member n hash trie bsize
  = member' 0 trie
  where
    member' :: Int -> Trie -> Bool
    member' lvl (Leaf xs) = n `elem` xs
    member' lvl (Node bv sns)
      | testBit bv index = memberSNode lvl (sns !! ones)
      | otherwise        = False
     where
       index = getIndex hash lvl bsize
       ones  = countOnesFrom index bv
     
    memberSNode :: Hash -> SubNode -> Bool
    memberSNode lvl (Term x)      = n == x
    memberSNode lvl (SubTrie t) = member' (lvl + 1) t

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf maxtd bsize val t
  = insert' val 0 t
  where
    insert' v lvl (Leaf vs)
      | v `elem` vs = Leaf vs
      | otherwise   = Leaf (v : vs)

    insert' v lvl (Node bv sns)
      | lvl == (maxtd - 1) = Leaf [v]
      | testBit bv index   = Node bv (replace ones sns sn')
      | otherwise          = Node (setBit bv index) (insertAt ones (Term v) sns)
      where
        index = getIndex (hf v) lvl bsize
        ones  = countOnesFrom index bv
        sn'   = newSNode (sns !! ones)

        newSNode :: SubNode -> SubNode
        newSNode (SubTrie t')
          = SubTrie (insert' v (lvl + 1) t')
        newSNode (Term v')
          | v == v'   = Term v'
          | otherwise = SubTrie (insert' v (lvl + 1) t')
          where
            t' = insert' v' (lvl + 1) empty

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf maxtd bsize xs
  = foldl (\t v -> insert hf maxtd bsize v t) empty xs