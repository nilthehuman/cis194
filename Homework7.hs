{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 7

module JoinList where

import Provided.Sized
import Provided.Buffer

import Control.Applicative ( liftA2 )
import Control.Arrow       ( (&&&) )

import Data.Char           ( toLower )
import Data.Foldable       ( foldMap )
import Data.Maybe          ( fromMaybe )
import Data.Monoid

-- Monoidally Annotated Join-Lists

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- The suggested helper function
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _)   = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ x     = x
x     +++ Empty = x
x     +++ y     = Append (tag x `mappend` tag y) x y

unsize :: Sized a => a -> Int
unsize = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ 0 (Single _ x)   = Just x
indexJ _ (Single _ _)   = Nothing
indexJ i (Append s l r)
     | i      <  0        = Nothing  -- out of bounds
     | unsize s <= i      = Nothing  -- out of bounds
     | i < unsize (tag l) = indexJ i                    l
     | otherwise          = indexJ (i - unsize (tag l)) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty
    | n <= 0    = Empty
    | otherwise = error "out of bounds"
dropJ n s@(Single _ _)
    | n <= 0    = s
    | n == 1    = Empty
    | otherwise = error "out of bounds"
dropJ n a@(Append s l r)
    | n <= 0        = a
    | n == unsize s = Empty
    | unsize s <  n = error "out of bounds"
    | n <= unsize (tag l) = (dropJ n l) +++ r
    | otherwise           = Empty       +++ (dropJ (n - unsize (tag l)) r)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty
    | n <= 0    = Empty
    | otherwise = error "out of bounds"
takeJ n s@(Single _ _)
    | n <= 0    = Empty
    | n == 1    = s
    | otherwise = error "out of bounds"
takeJ n a@(Append s l r)
    | n <= 0        = Empty
    | n == unsize s = a
    | unsize s <  n = error "out of bounds"
    | n <= unsize (tag l) = takeJ n l
    | otherwise           = l +++ takeJ (n - unsize (tag l)) r

newtype Score = Score { unscore :: Int }
    deriving (Eq, Num, Show)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

scoreTable :: [(Char, Score)]
scoreTable = [
               ('a', 1),
               ('b', 3),
               ('c', 3),
               ('d', 2),
               ('e', 1),
               ('f', 4),
               ('g', 2),
               ('h', 4),
               ('i', 1),
               ('j', 8),
               ('k', 5),
               ('l', 1),
               ('m', 3),
               ('n', 1),
               ('o', 1),
               ('p', 3),
               ('q',10),
               ('r', 1),
               ('s', 1),
               ('t', 1),
               ('u', 1),
               ('v', 4),
               ('w', 4),
               ('x', 8),
               ('y', 4),
               ('z',10)
             ]

score :: Char -> Score
score = fromMaybe mempty . flip lookup scoreTable . toLower

scoreString :: String -> Score
scoreString = foldMap score

scoreLine :: String -> JoinList Score String
scoreLine = liftA2 Single scoreString id

instance Buffer (JoinList (Score, Size) String) where
    toString Empty          = ""
    toString (Single _ s)   = s
    toString (Append _ l r) = toString l ++ toString r

    fromString = liftA2 Single (scoreString &&& Size . length) id

    line = indexJ

    replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n+1) jl

    numLines = unscore . fst . tag

    value    = unsize  . snd . tag

