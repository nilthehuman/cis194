{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 6

import Data.List ( intersperse )

-- Fibonacci numbers

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

fibs2' :: [Integer]
fibs2' = map fst $ iterate fibStep (0, 1)
    where fibStep (x, y) = (y, x + y)


-- Streams

infixr 4 :::

data Stream a = (:::) a (Stream a)

streamToList :: Stream a -> [a]
streamToList (x ::: xs) = x : streamToList xs

streamTake :: Int -> Stream a -> [a]
streamTake n = take n . streamToList

instance Show a => Show (Stream a) where
    show = ('[':) . (++ "...") . concat . intersperse "," . map show . streamTake 20

-- This is basically what Prelude.repeat does
streamRepeat :: a -> Stream a
streamRepeat x = let s = x ::: s in s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x ::: xs) = f x ::: streamMap f xs

-- Nothing can stop the unfold train this time!
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x ::: go f x
    where go f x = f x ::: go f (f x)

nats :: Stream Integer
nats = streamFromSeed succ 0

posInts :: Stream Integer
posInts = streamFromSeed succ 1

ruler :: Stream Integer
ruler = streamMap factor2 posInts
    where factor2 n = go n 0
          go n acc
             | n `mod` 2 == 0  = go (n `div` 2) (acc + 1)
             | otherwise       = acc


-- Fibonacci numbers via generating functions

x :: Stream Integer
x = 0 ::: (1 ::: streamRepeat 0)

instance Num (Stream Integer) where
    fromInteger = (::: streamRepeat 0)
    negate      = streamMap negate
    (x ::: xs) +   (y ::: ys) = x + y ::: xs + ys
    (x ::: xs) * b@(y ::: ys) = x * y ::: streamMap (x*) ys + xs * b

instance Fractional (Stream Integer) where
    (x ::: xs) / (y ::: ys) = let q = x `div` y ::: streamMap (`div` y) (xs - q * ys)
                                      in q

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)

-- Don't really feel like doing the matrix exercise just now

