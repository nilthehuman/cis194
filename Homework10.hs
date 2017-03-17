{-# OPTIONS_GHC -Wall #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 10

module Homework10 where

import Control.Applicative
import Control.Arrow       ( first )
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

inParser :: ((String -> Maybe (a, String))  ->
             (String -> Maybe (b, String))) ->
             Parser a ->
             Parser b
inParser = (Parser .) . (. runParser)

instance Functor Parser where
    fmap f = inParser $ fmap fmap fmap (first f)

instance Applicative Parser where
    pure x    = Parser $ Just . (x,)
    pf <*> px = Parser $ \ys -> case runParser pf ys of
                                     Just (f, ys') -> first f <$> runParser px ys'
                                     _             -> Nothing

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = pure (pure ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ y -> [x,y]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
    empty     = Parser $ const Nothing
    p1 <|> p2 = Parser $ \xs -> runParser p1 xs <|> runParser p2 xs

mute :: Parser a -> Parser ()
mute = fmap $ pure ()

intOrUppercase :: Parser ()
intOrUppercase = mute posInt <|> mute (satisfy isUpper)

