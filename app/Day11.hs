module Main where

import AoC qualified
import Control.Arrow ((>>>))
import Data.IntMap.Strict qualified as Map

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ words >>> map read
  where
    solvePart1 = iterate (concatMap step) >>> (!! 25) >>> length
    solvePart2 =
      map (,1) >>> Map.fromListWith (+) >>> iterate stepMap >>> (!! 75) >>> Map.elems >>> sum

step :: Int -> [Int]
step 0 = [1]
step n =
  let digitCount = floor (logBase 10 (fromIntegral n) + 1)
   in if even digitCount
        then
          let (l, r) = splitAt (digitCount `quot` 2) (show n)
           in map read [l, r]
        else [n * 2024]

stepMap :: Map.IntMap Int -> Map.IntMap Int
stepMap =
  Map.assocs
    >>> concatMap (\(stone, count) -> map (,count) $ step stone)
    >>> Map.fromListWith (+)
