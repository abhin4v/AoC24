module Main where

import AoC qualified
import Data.List (sort, transpose)

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ \input ->
  let [list1, list2] = map sort . transpose . map (map read . words) . lines $ input
   in (list1, list2)
  where
    solvePart1 (list1, list2) = sum $ zipWith (\x y -> abs $ x - y) list1 list2
    solvePart2 (list1, list2) = sum [c * n | n <- list1, let c = length $ filter (== n) list2]
