{-# OPTIONS_GHC -Wall #-}

--  !!  WARNING: SPOILERS AHEAD  !!  --

-- CIS 194: Homework 2

-- Log file parsing

module LogAnalysis where

import Provided.Log

import Control.Applicative ( liftA2 )
import Data.Function       ( on )
import Data.List           ( unfoldr )
import Text.Read           ( readMaybe )

-- This definition feels really clumsy, especially the "error" case.
-- There must be a more elegant way to do this
parseMessage :: String -> LogMessage
parseMessage l@('I':msg)
    | null msg  = Unknown l
    | otherwise = let (t:rest) = words msg
                      time = readMaybe t :: Maybe Int
                      in case time of Nothing -> Unknown l
                                      Just t  -> LogMessage Info t (unwords rest)
parseMessage l@('W':msg)
    | null msg  = Unknown l
    | otherwise = let (t:rest) = words msg
                      time = readMaybe t :: Maybe Int
                      in case time of Nothing -> Unknown l
                                      Just t  -> LogMessage Warning t (unwords rest)
parseMessage l@('E':msg)
    | (length . words) msg < 2 = Unknown l
    | otherwise                = let (s:t:rest) = words msg
                                     svty = readMaybe s :: Maybe Int
                                     time = readMaybe t :: Maybe Int
                                     in case liftA2 (,) svty time
                                             of Nothing    -> Unknown l
                                                Just (s,t) -> if not $ liftA2 (&&) (0<=) (<=100) s
                                                              then Unknown l
                                                              else LogMessage (Error s) t (unwords rest)
parseMessage msg = Unknown msg

-- LogMessage is not defined with record syntax
timeStamp :: LogMessage -> Maybe TimeStamp
timeStamp (LogMessage _ t _) = Just t
timeStamp (Unknown _)        = Nothing

message :: LogMessage -> String
message (LogMessage _ _ m) = m
message (Unknown m)        = m

-- The exercise doesn't expect us to rebalance the MessageTree so 'insert' is easy
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t    = t
insert x           Leaf = Node Leaf x Leaf
insert x           (Node l y r)
    | (compare `on` timeStamp) x y == LT  = Node (insert x l) y r
    | otherwise                           = Node l            y (insert x r)

-- Combinators are awesome!
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder = unfoldr tearDown
    where
        tearDown Leaf             = Nothing
        tearDown (Node Leaf x r ) = Just (x, r)
        tearDown (Node l    x r ) = let Just (y, l') = tearDown l in Just (y, Node l' x r)


-- Log file postmortem

relevant :: LogMessage -> Bool
relevant (LogMessage (Error s) _ _) = 50 <= s
relevant _                          = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . inOrder . build . filter relevant

-- Brent provides a small test case in the exercise description:
smallTestMsgs :: [String]
smallTestMsgs = [ "I 6 Completed armadillo processing"
                , "I 1 Nothing to report"
                , "E 99 10 Flange failed!"
                , "I 4 Everything normal"
                , "I 11 Initiating self-destruct sequence"
                , "E 70 3 Way too many pickles"
                , "E 65 8 Bad pickle-flange interaction detected"
                , "W 5 Flange is due for a check-up"
                , "I 7 Out for lunch, back in two time steps"
                , "E 20 2 Too many pickles"
                , "I 9 Back from lunch" ]

smallTestExpected :: [String]
smallTestExpected = [ "Way too many pickles"
                    , "Bad pickle-flange interaction detected"
                    , "Flange failed!" ]

runSmallTest :: Bool
runSmallTest = smallTestExpected == (whatWentWrong . map parseMessage $ smallTestMsgs)

runBigTest :: IO [String]
runBigTest = fmap (whatWentWrong . map parseMessage . lines) . readFile $ "Provided/error.log"

-- Answer to exercise 6:
-- Apparently the Mad Hatter? In the Alice in Wonderland animated film
-- he refused to put mustard into the Rabbit's watch.

