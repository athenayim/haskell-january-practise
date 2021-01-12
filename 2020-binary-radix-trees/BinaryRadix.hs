module Radix where
  
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

type RadixTree = Tree Bool

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative

-- Computes bytes used to represent IntTree
-- Empty - 1 byte, Leaf - 5 bytes, Node - 13 bytes
size :: IntTree -> Int
size Empty = 1
size (Leaf _) = 5
size (Node _ l r) = 13 + size l + size r

-- Computes bytes used to represent RadixTree
-- Leaf - 1 byte, Node - 9 byes
size' :: RadixTree -> Int
size' (Leaf _) = 1
size' (Node _ l r) = 9 + size' l + size' r

-- Converts integer to its binary representation
binary :: Int -> BitString
binary 0 = [0]
binary 1 = [1]
binary n = binary (n `div` 2) ++ binary (n `mod` 2)

-- Adds a bit string to a radix tree
insert :: BitString -> RadixTree -> RadixTree
insert [] (Node _ l r) = Node True l r
insert [] (Leaf _) = Leaf True
insert (x : xs) (Node n l r)
 | x == 0    = Node n (insert xs l) r
 | otherwise = Node n l (insert xs r)
insert xs (Leaf n) = insert xs (Node n (Leaf False) (Leaf False))

-- Builds radix tree from list of ints
buildRadixTree :: [Int] -> RadixTree
buildRadixTree xs
 = foldr (insert . binary) (Leaf False) xs

-- Checks whether an int is in a radix tree
member :: Int -> RadixTree -> Bool
member n t
  = member' (binary n) t
  where
    member' :: BitString -> RadixTree -> Bool
    member' [] (Node x _ _) = x
    member' [] (Leaf x) = x
    member' (x : xs) (Node n l r)
     | x == 0    = member' xs l
     | otherwise = member' xs r
    member' (x : xs) (Leaf n) = n

-- Computes union of two radix trees
union :: RadixTree -> RadixTree -> RadixTree
union (Leaf False) t = t
union t (Leaf False) = t
union (Leaf n1) (Leaf n2) = Leaf (n1 || n2)
union (Node n1 l1 r1) (Node n2 l2 r2)
 = Node (n1 || n2) (union l1 l2) (union r1 r2)

-- Computes intersection of two radix trees
intersection :: RadixTree -> RadixTree -> RadixTree
intersection (Leaf False) _ = Leaf False
intersection _ (Leaf False) = Leaf False
intersection (Leaf n1) (Leaf n2) = Leaf (n1 && n2)
intersection (Node n1 l1 r1) (Node n2 l2 r2)
 = Node (n1 && n2) (intersection l1 l2) (intersection r1 r2)

-- Returns size of IntTree and RadixTree of a given length of [Int]
compareTrees :: Int -> (Int, Int)
compareTrees n
 = (intSize, radixSize)
 where
   ns = take n rs
   intSize = size (buildIntTree ns)
   radixSize = size' (buildRadixTree ns)

-- CONCLUSION: The break-even point is 205.
-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node False (Leaf True)
               (Node True (Leaf False)
                          (Node True (Node False (Leaf True)
                                                 (Leaf False))
                                     (Leaf True)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node False (Node False (Leaf True)
                            (Node True (Leaf False) (Leaf True)))
                (Leaf True)

