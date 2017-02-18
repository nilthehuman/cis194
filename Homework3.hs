{-# OPTIONS_GHC -Wall #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 3

-- Code golf!

module Golf where

import Data.List ( elemIndices, transpose, unfoldr )

-- unfoldr is a standard library function that generates a list from an initial
-- seed value by repeatedly applying the same function until it signals
-- termination.
skips :: [a] -> [[a]]
skips l = unfoldr s ([], 1, 1, l)
    where s (a, k, n, x)  -- uncurried for the sake of brevity
            | length l < n = Nothing                            -- unfoldr terminates here
            | null x       = Just (a, ([], n+1, n+1, l))        -- we can refer back to the original l from the top-level function
            | k < 2        = s (a ++ [head x], n  , n, tail x)  -- we can't get rid of head/tail because (x:y) would not match []
            | True         = s (a            , k-1, n, tail x)  -- drop the next element by default

-- zip3 is a Prelude function of the "zip" family. It takes three lists and
-- forms triples out of them according to index from the left. Here we use it to
-- generate all consecutive triplets in the input list.
localMaxima :: [Integer] -> [Integer]
localMaxima l = map    (\ (_, y, _) -> y) .            -- return middle element only
                filter (\ (x, y, z) -> max x z < y) $  -- find triples that satisfy our predicate
                zip3 l (tail l) (drop 2 l)             -- construct all consecutive triples

-- An alternative but slightly longer solution.
-- Combinators that capture iteration are such an elegant feature
-- of functional languages!
localMaxima' :: [Integer] -> [Integer]
localMaxima' l = foldr b [] $ unfoldr f l
    where -- 'f' filters out the compliant values
          f (x:y:z:zs)
            | max x z < y = Just (Just y , y:z:zs)
            | True        = Just (Nothing, y:z:zs)  -- otherwise == True by definition
          f _             = Nothing                 -- unfoldr terminates here
          -- 'b' stands for an upside down 'p' as in "pure": 'b' unwraps a value from its context
          b Nothing  r    = r
          b (Just y) r    = y:r

-- The exercise fits Haskell so well that this solution is basically
-- self-explanatory. We stack the *'s to the /right/ though because
-- we want them to end up at the bottom when we transpose them.
histogram :: [Integer] -> String
histogram l = ((++) "==========\n0123456789\n") .  -- turns out aliasing replicate to 'r' doesn't pay off
              unlines .
              filter (elem '*') .
              transpose .
              -- The Rules say whitespace does not count so this trick beats
              -- the vanilla   replicate (10-x) ' ' ++ replicate x '*'
              map ((\x -> drop x "          " ++ replicate x '*') . length . (`elemIndices` l)) $
              [0..9]

