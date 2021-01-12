module Regex where

import Data.Maybe
import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- Text representation of regex expression

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp i (x : xs)
  | i == fst x = snd x
  | otherwise  = lookUp i xs

simplify :: RE -> RE
simplify (Plus re)     = Seq (simplify re) (Rep (simplify re))
simplify (Opt re)      = Alt (simplify re) Null
simplify (Seq re1 re2) = Seq (simplify re1) (simplify re2)
simplify (Alt re1 re2) = Alt (simplify re1) (simplify re2)
simplify (Rep re)      = Rep (simplify re)
simplify re            = re

--------------------------------------------------------
-- Part II

-- Automata indexing functions
startState :: Automaton -> State
startState (s, _, _) = s

terminalStates :: Automaton -> [State]
terminalStates (_, ts, _) = ts

transitions :: Automaton -> [Transition]
transitions (_, _, trs) = trs

isTerminal :: State -> Automaton -> Bool
isTerminal st atm
  = st `elem` terminalStates atm

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom st atm
  = trns (transitions atm)
  where
    trns [] = []
    trns (t@(st1, st2, l) : ts)
      | st == st1 = t : trns ts
      | otherwise = trns ts

labels :: [Transition] -> [Label]
labels ts
  = nub (labels' ts)
  where
    labels' [] = []
    labels' ((st1, st2, Eps) : ts) = labels' ts
    labels' ((st1, st2, l) : ts)   = l : labels' ts

accepts :: Automaton -> String -> Bool
accepts atm s
  = accepts' (startState atm) s
  where
    accepts' :: State -> String -> Bool 
    accepts' st s
      | isTerminal st atm && null s = True
      | otherwise = any (try s) (transitionsFrom st atm)
      where
        try :: String -> Transition -> Bool
        try s (st1, st2, Eps) = accepts' st2 s
        try "" (st1, st2, C ch) = False
        try (c : cs) (st1, st2, C ch)
          | c == ch   = accepts' st2 cs
          | otherwise = False

--------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

-- Constructs NDA from state m to n
make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null m n k 
  = ([(m, n, Eps)], k)
make (Term ch) m n k 
  = ([(m, n, C ch)], k)
make (Seq r1 r2) m n k 
  = (r1' ++ (k, k + 1, Eps) : r2', k'')
  where
    (r1', k') = make r1 m k (k + 2)
    (r2', k'') = make r2 (k + 1) n k'
make (Alt r1 r2) m n k
  = ((m, k, Eps) : (m, k + 2, Eps) : (k + 1, n, Eps) : (k + 3, n, Eps) : r1' ++ r2', k'')
  where
    (r1', k') = make r1 k (k + 1) (k + 4)
    (r2', k'') = make r2 (k + 2) (k + 3) k'
make (Rep r) m n k
  = ((m, k, Eps) : (m, n, Eps) : (k + 1, k, Eps) : (k + 1, n, Eps) : r', k')
  where
    (r', k') = make r k (k + 1) (k + 2)


--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier
  = undefined

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions
  = undefined

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA 
  = undefined

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

