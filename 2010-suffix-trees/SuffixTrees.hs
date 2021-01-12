module SuffixTrees where

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

-- Returns whether first string is a prefix of the second
isPrefix :: String -> String -> Bool
isPrefix "" s = True
isPrefix s "" = False
isPrefix (x : xs) (y : ys)
  | x == y    = isPrefix xs ys
  | otherwise = False

-- Removes prefix from string
--Pre: s is a prefix of s'
removePrefix :: String -> String -> String
removePrefix "" s' = s'
removePrefix s s'  = removePrefix (tail s) (tail s')

-- Returns list of suffixes of a string
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s  = s : suffixes (tail s)

-- Returns whether s is a substring of s'
isSubstring :: String -> String -> Bool
isSubstring s s' = any (isPrefix s) (suffixes s')

-- Returns indices of all occurrences of a substring in a string
-- Uses naive approach
findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = map fst (filter snd numsuffix)
  where
    numsuffix = zip [0..] (map (isPrefix s) (suffixes s'))

------------------------------------------------------

-- Returns indices stored in leaves of suffix tree
getIndices :: SuffixTree -> [Int]
getIndices (Leaf x)        = [x]
getIndices (Node [])       = []
getIndices (Node (x : xs)) = getIndices (snd x) ++ getIndices (Node xs)

-- Extracts common prefix from two given strings
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition s s'
  = partition' s s' []
  where
    partition' [] st p = (p, [], st)
    partition' st [] p = (p, st, [])
    partition' st@(x : xs) st'@(y : ys) p
     | x == y    = partition' xs ys (p ++ [x])
     | otherwise = (p, st, st')

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf _) = []
findSubstrings' _ (Node []) = []
findSubstrings' s (Node (x : xs))
  | s' == ""  = getIndices t
  | a' == ""  = findSubstrings' s' t
  | otherwise = findSubstrings' s (Node xs)
  where
    (a, t) = x
    (p, s', a') = partition s a

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert _ (Leaf x)       = Leaf x
insert (s, i) (Node []) = Node [(s, Leaf i)]
insert n@(s, i) (Node (x : xs))
  | p /= "" && p == a = Node ((a, insert (s', i) t) : xs)
  | p /= "" && p /= a = Node ((p, Node [(s', Leaf i), (a', t)]) : xs)
  | otherwise         = Node (x : xs')
  where
    (a, t) = x
    (p, s', a') = partition s a
    Node xs' = insert n (Node xs)


-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


