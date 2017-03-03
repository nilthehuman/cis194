{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 5

module Calc where

import Provided.ExprT
import Provided.Parser
import qualified Provided.StackVM as S

import Data.Composition ( (.:) )
import qualified Data.Map as M

-- Expressions

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (0<)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add = (`mod` 7) .: (+)
    mul = (`mod` 7) .: (*)

newtype MinMax = MinMax Integer deriving (Enum, Eq, Integral, Num, Ord, Real, Show)
newtype Mod7   = Mod7   Integer deriving (Enum, Eq, Integral, Num, Ord, Real, Show)


-- Custom CPU

-- The problem text gets really confusing around here:

-- "Simply create an instance of the Expr type class for Program, so that arithmetic
-- expressions can be interpreted as compiled programs."

-- Pretty sure he meant the other way around.

-- "For any arithmetic expression exp :: Expr a => a it should be the case that
--   stackVM exp == Right [IVal exp]"

-- That's not even possible. stackVM :: [StackExp] -> Either String StackVal,
-- it can't take an arbitrary Expr. What's going on here?
instance Expr S.Program where
    lit = (:[]) . S.PushI
    add = (++ [S.Add]) .: (++)
    mul = (++ [S.Mul]) .: (++)

compile :: String -> Maybe S.Program
compile = parseExp lit add mul


-- Introducing variables

class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit     = const . Just
    add     = liftA2 . liftA2 $ (+)
    mul     = liftA2 . liftA2 $ (*)

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- Provided in the problem description
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs x = x $ M.fromList vs

