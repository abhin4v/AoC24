module Main where

import AoC qualified
import Data.List.HT (mapAdjacent, removeEach)
import Data.Ord.HT (inRange)

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ map (map read . words) . lines
  where
    solvePart1 = length . filter isSafe
    solvePart2 = length . filter (any (isSafe . snd) . removeEach)

    isSafe levels =
      any allAdjacentLevels [(<), (>)]
        && allAdjacentLevels (\l1 l2 -> inRange (1, 3) $ abs $ l1 - l2)
      where
        allAdjacentLevels f = and . mapAdjacent f $ levels
