module Main where

import AoC qualified
import Control.Arrow ((>>>))
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Massiv.Array qualified as A

type Input = A.Array A.P Ix2 Char

main :: IO ()
main =
  AoC.main solvePart1 solvePart2 $
    lines
      >>> map (\row -> "..." <> row <> "...")
      >>> ( \rows ->
              let padding = replicate 3 $ replicate (length $ head rows) '.'
               in padding <> rows <> padding
          )
      >>> A.fromLists' A.Seq

solvePart1 :: Input -> Int
solvePart1 input =
  length
    [ ()
      | let Sz2 rowCount colCount = A.size input,
        row <- [0 .. rowCount - 4],
        col <- [0 .. colCount - 4],
        let subGrid = A.compute @A.P $ A.extract' (row :. col) (Sz $ 4 :. 4) input,
        word <-
          map
            (map (subGrid A.!) . flip map [0 .. 3])
            [(0 :.), (:. 0), \i -> i :. i, \i -> 3 - i :. i],
        word `elem` ["XMAS", "SAMX"]
    ]

solvePart2 :: Input -> Int
solvePart2 input =
  length
    [ ()
      | let Sz2 rowCount colCount = A.size input,
        row <- [0 .. rowCount - 3],
        col <- [0 .. colCount - 3],
        let subGrid = A.compute @A.P $ A.extract' (row :. col) (Sz $ 3 :. 3) input,
        let word = map ((subGrid A.!) . uncurry Ix2) masIdxs,
        word `elem` ["MMASS", "MSAMS", "SMASM", "SSAMM"]
    ]
  where
    masIdxs = [(0, 0), (0, 2), (1, 1), (2, 0), (2, 2)]
