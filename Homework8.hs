{-# OPTIONS_GHC -Wall #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 8

module Party where

import Provided.Employee

import Control.Applicative ( (<$>) )
import Data.Biapplicative  ( biliftA2 )
import Data.Function       ( on )
import Data.List           ( sort )
import Data.Monoid
import Data.Tree
import System.IO           ( withFile, hGetContents, Handle, IOMode(..) )

-- Preliminaries

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

-- Pity I can't just do this: (maybe with FlexibleInstances though?)
-- instance Biapplicative GuestList where
--     bipure = GL
--     (GL f g) <<*>> (GL es fun) = GL (f es) (g fun)

-- We need to unwrap our GuestList in order to make it a Biapplicative.
unGL :: GuestList -> ([Employee], Fun)
unGL (GL es fun) = (es, fun)

instance Monoid GuestList where
    mempty      = GL [] 0
    mappend x y = uncurry GL $ biliftA2 (++) (+) (unGL x) (unGL y)

instance Ord GuestList where
    compare = compare `on` snd . unGL

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- I'm pretty sure this is a standard "cata" of some sort but I'm not sure
-- how to use the relevant combinators, so it's gonna be vanilla recursion
-- for now.
-- ...Wait, it's just "list all the nodes and fold them up", right?
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f z = foldr f z . flatten

-- Wait, no, they want one that goes level-by-level (see next Exercise).
treeFold' :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold' f zs (Node x ts) = f x $ map (treeFold' f zs) ts


-- The algorithm

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel r gs = let (withBoss, withoutBoss) = unzip gs
                     withBoss'    = r `glCons` mconcat withoutBoss
                     withoutBoss' =            mconcat withBoss
                     in (withBoss', withoutBoss')
                     -- Language is expressive as fuck...

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold' nextLevel []


-- The whole company

companyFile :: FilePath
companyFile = "./Provided/company.txt"

readCompany :: FilePath -> IO (Tree Employee)
readCompany f = withFile f ReadMode go
    where go :: Handle -> IO (Tree Employee)
          go = fmap read <$> hGetContents

prettyPrint :: GuestList -> [String]
prettyPrint gs = let (es, fun) = unGL gs
                 in ("Total fun: " ++ show fun) :
                    (sort . map empName) es

main :: IO ()
main = readCompany companyFile >>= mapM_ putStrLn . prettyPrint . maxFun

