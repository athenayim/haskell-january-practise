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

buildRadixTree :: [Int] -> RadixTree
buildRadixTree
  = undefined

member :: Int -> RadixTree -> Bool
member
  = undefined

union :: RadixTree -> RadixTree -> RadixTree
union
  = undefined

intersection :: RadixTree -> RadixTree -> RadixTree
intersection
  = undefined

-- CONCLUSION: The break-even point is xxx.

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
