module Main where

import AoC qualified
import AoC.List (ordNub)
import Control.Arrow ((&&&), (>>>))
import Data.Char (digitToInt)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Tree qualified as Tree

main :: IO ()
main = AoC.main solvePart1 solvePart2 parse
  where
    solvePart1 = trailEnds >>> map (ordNub >>> length) >>> sum
    solvePart2 = trailEnds >>> map length >>> sum

    parse =
      lines
        >>> zip [0 ..]
        >>> map (\(i, row) -> zipWith (\j c -> ((j, i), digitToInt c)) [0 ..] row)
        >>> (\rows -> (Map.fromList $ concat rows, length (head rows), length rows))

    trailEnds (grid, width, height) =
      Map.assocs grid
        & filter ((== 0) . snd)
        & Tree.unfoldForest (id &&& nexts)
        & map (Tree.levels >>> (!! 9))
      where
        nexts ((x, y), alt) =
          [ (point', alt')
            | (x', y') <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)],
              x' >= 0 && x' < width && y' >= 0 && y' < height,
              let point' = (x', y'); alt' = grid Map.! point',
              alt' == alt + 1
          ]
