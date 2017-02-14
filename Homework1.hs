{-# OPTIONS_GHC -Wall #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 1

-- Validating credit card numbers

import Control.Applicative ( (<$>), liftA2, pure )
import Control.Monad       ( join )

import Data.Char           ( ord )
import Data.Foldable as F  ( or )
import Data.List           ( elemIndex, unfoldr )
import Data.Maybe          ( fromJust )

import System.Random       ( getStdGen, randomRs )

-- Simply convert to String and read each digit to get the list of digits
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = fmap ((fromIntegral . subtract (ord '0')) . ord) . show $ x
    -- fromIntegral is only necessary because ord :: Char -> Int

-- Alternatively, we can do it the arithmetic way
toDigitsRev :: Integer -> [Integer]
toDigitsRev = unfoldr (\x -> if 0 < x
                                then Just (x `mod` 10, x `div` 10)
                                else Nothing)

-- A useful helper function: apply f to every nth element.
-- A negative n means counting from the right (the back of the list)
every :: Integer -> (a -> a) -> [a] -> [a]
every n f xs
    | n < 0     = reverse . every (-n) f . reverse $ xs
    | otherwise = go n 1 f xs
    where go _ _ _ [] = []
          go n k f (x:xs)
           | n == k    = (f x) : go n    1  f xs
           | otherwise =    x  : go n (k+1) f xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = every (-2) (*2)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (0 ==) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- The Towers of Hanoi

type Peg  = String
type Move = (Peg, Peg)

-- Let's look at the first couple of cases:
-- hanoi 1 x y z == [(x,z)]
-- hanoi 2 x y z == [(x,y), (x,z), (y,z)]                             == (hanoi 1 x z y) ++ [(x,z)] ++ (hanoi 1 y x z)
-- hanoi 3 x y z == [(x,z), (x,y), (z,y), (x,z), (y,x), (y,z), (x,z)] == (hanoi 2 x z y) ++ [(x,z)] ++ (hanoi 2 y x z)
-- ...

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n x y z
    | n < 0     = error "negative number of discs"
    | otherwise = (hanoi (n-1) x z y) ++ [(x,z)] ++ (hanoi (n-1) y x z)

-- For fun: let's make a function that tries random Moves until it gets there
untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f x = (x >>= p) >>= (\b -> if b
                                       then x
                                       else untilM p f (x >>= f))

blindHanoi :: Integer -> Peg -> Peg -> Peg -> IO [Move]
blindHanoi n x y z = join $ sequence . reverse . snd <$> untilM (fmap (winningState ==) . sequence . fst)
                                                                play
                                                                (pure (map pure initialState, []))
    where initialState  = replicate (fromIntegral n) x
          winningState  = replicate (fromIntegral n) z
          randomMoves   = let packPairs (a:b:bs) = (a,b):packPairs bs
                              in packPairs . map ([x,y,z] !!) . randomRs (0,2) <$> getStdGen
          legal s (p,q) = let d = elemIndex p s
                              in p /= q && (F.or $ notElem q . flip take s <$> d)
          play (ioS,ms) = do
                          s <- sequence ioS
                          let m = head . filter (legal s) . drop (length ms) <$> randomMoves
                              d = fromJust . flip elemIndex s . fst <$> m   -- m is legal => d exists => fromJust is OK
                              splitS = (liftA2 splitAt) d (pure s)          -- I think I'm supposed to use lenses here?
                          (smaller, _:bigger) <- splitS
                          pure (map pure smaller ++ [snd <$> m] ++ map pure bigger, m:ms)

-- An easier Hanoi game variant with 4 pegs. The solution is far less obvious though.
-- hanoi4 1 w x y z == [(w,z)]
-- hanoi4 2 w x y z == [(w,x), (w,z), (x,z)]   (more than one optimal solution from here on)
-- hanoi4 3 w x y z == [(w,y), (w,x), (w,z), (x,z), (y,z)]
-- hanoi4 4 w x y z == [(w,x), (w,y), (x,y), (w,x), (w,z), (x,z), (y,x), (y,z), (x,z)]
-- hanoi4 5 w x y z == [(w,y), (w,x), (y,x), (w,z), (w,y), (z,y), (w,z), (y,w), (y,z), (w,z), (x,y), (x,z), (y,z)]
-- ...
-- No obvious pattern arises. Let's use the following scheme:
-- 1. Stack the top half of the disc tower on peg x.
-- 2. Stack the rest but not the bottom disc on peg y without affecting peg x.
-- 3. Move the bottom disc to peg z without affecting peg x.
-- 4. Stack the discs from step 2 on peg z.
-- 5. Stack the discs from step 1 on peg z.

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n w x y z
    | n < 0     = error "negative number of discs"
    | otherwise = hanoi4 (n `div` 2) w y z x ++ hanoi3 (n - n `div` 2 - 1) w z y ++
                  [(w,z)] ++
                  hanoi3 (n - n `div` 2 - 1) y w z ++ hanoi4 (n `div` 2) x w y z
                  where hanoi3 = hanoi

