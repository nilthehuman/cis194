{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 12

module Homework12 where

import Control.Applicative  ( (<$>), (<*>), liftA2 )
import Control.Monad        ( replicateM )
import Control.Monad.Random
import Data.Composition     ( (.:) )
import Data.Foldable        ( Foldable, foldMap )
import Data.Monoid
import Data.List            ( sortBy )

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = getSum .: foldMap $ \x -> if p x
                                       then Sum 1
                                       else Sum 0

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = Battlefield <$> ((a -) <$> deadAs) <*> ((d -) <$> deadDs)
    where as      = sortBy (flip compare) <$> replicateM (min 3 a) die
          ds      = sortBy (flip compare) <$> replicateM (min 2 d) die
          outcome = zipWith (<=) <$> as <*> ds
          deadAs  = count id  <$> outcome
          deadDs  = count not <$> outcome

-- Reused from a bonus exercise in Homework 1
untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f x = (x >>= p) >>= (\b -> if b
                                       then x
                                       else untilM p f (x >>= f))

invade :: Battlefield -> Rand StdGen Battlefield
invade = untilM done battle . return
    where done (Battlefield a d) = return $ a < 2 || d < 1

success :: Battlefield -> Bool
success (Battlefield _ 0) = True
success _                 = False

successProb :: Battlefield -> Rand StdGen Double
successProb = fmap (prob success) . replicateM 1000 . invade
    where prob f = liftA2 (/) (fromIntegral . count f) (fromIntegral . length)

