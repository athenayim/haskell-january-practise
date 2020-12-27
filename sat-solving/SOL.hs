module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- Looks up item in a list of item-value pairs
-- Pre: The item being looked up has a unique binding in the list
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x (y : ys)
  | x == fst y = snd y
  | otherwise  = lookUp x ys

-- Returns sorted list of variable names in a formula without duplicates
vars :: Formula -> [Id]
vars (Var x)   = [x]
vars (Not x)   = nub (sort (vars x))
vars (And x y) = nub (sort (vars x ++ vars y))
vars (Or x y)  = nub (sort (vars x ++ vars y))

-- Generates list of Id-Int pairs
idMap :: Formula -> IdMap
idMap f = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- Converts formula to Negation Normal Form
toNNF :: Formula -> NNF
toNNF (Var x)         = Var x
toNNF (Not (Var x))   = Not (Var x)
toNNF (Not (Not x))   = x
toNNF (Not (And x y)) = Or (toNNF (Not x)) (toNNF (Not y))
toNNF (Not (Or x y))  = And (toNNF (Not x)) (toNNF (Not y))
toNNF (And x y)       = And (toNNF x) (toNNF y)
toNNF (Or x y)        = Or (toNNF x) (toNNF y)

-- Converts formula to Conjunctive Normal Form
toCNF :: Formula -> CNF
toCNF f
  = toCNF' (toNNF f)
  where
    toCNF' (Or x y) = distribute x y
    toCNF' f        = f

-- Gives flattened representation of CNF formula
flatten :: CNF -> CNFRep
flatten f
  = flatten' f
  where
    ids = idMap f

    flatten' (Var x) = [[lookUp x ids]]
    flatten' (Not (Var x)) = [[negate (lookUp x ids)]]
    flatten' (And x y) = flatten' x ++ flatten' y
    flatten' (Or x y) = [concat (flatten' x ++ flatten' y)]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits 
  = undefined

-- 4 marks
dp :: CNFRep -> [[Int]]
dp 
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined


