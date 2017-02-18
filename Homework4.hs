{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 4

import Control.Arrow ( (&&&) )

-- Wholemeal programming

-- So easy... Must... resist... urge to lecture coworkers...
-- on the advantages of the purely functional paradigm...
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = fst . until ((1 ==) . snd) f . (0,)
    where f (acc, n)
            | even n    = (acc + n, n `div` 2)
            | otherwise = (acc    , 3 * n + 1)

-- Or, equivalently, using takeWhile and iterate as suggested
-- in the problem text:
fun2'' :: Integer -> Integer
fun2'' = sum . filter even . takeWhile (1 /=) . iterate f
    where f n
            | even n    = n `div` 2
            | otherwise = 3 * n + 1

-- This should be True
testFun2Equivalence :: Bool
testFun2Equivalence = and . map (uncurry (==) . (fun2' &&& fun2'')) $ [1..10000]


-- Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- A handy helper function
height :: Tree a -> Integer
height Leaf                 = 0
height (Node _ Leaf _ Leaf) = 0
height (Node _ l    _ r)    = 1 + max (height l) (height r)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where insert x Leaf           = Node 0 Leaf x Leaf
          insert x (Node h l y r)
            | height l > height r = Node h     l            y (insert x r)
            | height l < height r = Node h     (insert x l) y r
            | otherwise           = Node (h+1) (insert x l) y r


-- More folds!

-- The problem statement asked for a fold, so...
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- ...but this xor is easier to read:
xor' :: [Bool] -> Bool
xor' = odd . length . filter (True ==)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b = foldr (flip f) b . reverse

-- Finding primes

-- Thanks to composition and list comprehensions we can implement the algorithm
-- almost verbatim.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) . filter p $ [1..n]
    where p x = null [ () | i <- [1..n],
                            j <- [1..n],
                            i <= j,
                            i + j + 2*i*j <= n,
                            x == i + j + 2*i*j ]

