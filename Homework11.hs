{-# OPTIONS_GHC -Wall #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 11

module Homework11 where

import Homework10
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p $ zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

negInt :: Parser Integer
negInt = char '-' *> (negate <$> posInt)

integer :: Parser Integer
integer = posInt <|> negInt

open :: Parser Char
open = char '('

close :: Parser Char
close = char ')'

atom :: Parser Atom
atom = N <$> integer <|> I <$> ident

comb :: Parser [SExpr]
comb = open *> oneOrMore parseSExpr <* close

-- So damn clean.
parseSExpr :: Parser SExpr
parseSExpr = spaces *> (Comb <$> comb <|> A <$> atom) <* spaces

