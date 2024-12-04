module AoC.List where

import Data.Set qualified as Set

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = case f x of
  Nothing -> [x]
  Just y -> x : iterateMaybe f y

ordNub :: (Ord a) => [a] -> [a]
ordNub = ordNubBy id

ordNubBy :: (Ord b) => (a -> b) -> [a] -> [a]
ordNubBy f = go Set.empty
  where
    go s = \case
      [] -> []
      (x : xs) | Set.member (f x) s -> go s xs
      (x : xs) -> x : go (Set.insert (f x) s) xs

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows size list
  | size > length list = []
  | otherwise = take size list : slidingWindows size (tail list)
