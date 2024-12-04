module Main where

import AoC qualified
import AoC.List (iterateMaybe, ordNub, slidingWindows)
import Data.Function ((&))
import Data.Massiv.Array qualified as A
import Math.Geometry.Grid qualified as G
import Math.Geometry.Grid.OctagonalInternal qualified as G

type Input = A.Array A.U A.Ix2 Char

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ A.fromLists' A.Seq . lines

solvePart1 :: Input -> Int
solvePart1 input =
  let A.Sz2 rowCount colCount = A.size input
      grid = G.rectOctGrid rowCount colCount
   in grid
        & G.indices
        & concatMap (\idx -> map (vista grid idx) directions)
        & concatMap (slidingWindows 4)
        & ordNub
        & map (map (input A.!))
        & filter (== "XMAS")
        & length
  where
    vista grid idx dir =
      map (uncurry A.Ix2) $ iterateMaybe (flip (G.neighbour grid) dir) idx
    directions =
      [G.West, G.Northwest, G.North, G.Northeast, G.East, G.Southeast, G.South, G.Southwest]

solvePart2 :: Input -> Int
solvePart2 input =
  let A.Sz2 rowCount colCount = A.size input
   in length $
        [ subGridStr
          | row <- [0 .. rowCount - 3],
            col <- [0 .. colCount - 3],
            let subGrid = A.compute @A.U $ A.extract' (row A.:. col) (A.Sz (3 A.:. 3)) input,
            let subGridStr = map ((subGrid A.!) . uncurry A.Ix2) masIdxs,
            subGridStr `elem` ["MMASS", "MSAMS", "SMASM", "SSAMM"]
        ]
  where
    masIdxs = [(0, 0), (0, 2), (1, 1), (2, 0), (2, 2)]
