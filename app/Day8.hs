module Main where

import AoC qualified
import AoC.List (ensure, iterateMaybe, ordNub)
import Control.Arrow ((>>>))
import Data.Function (on, (&))
import Data.List (sortOn)
import Data.List.HT (groupBy)

type Point = (Int, Int)

type Input = (Int, Int, [[Point]])

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ lines >>> parseLines
  where
    solvePart1 = solve (\(ns1, ns2) -> take 1 ns1 <> take 1 ns2) False
    solvePart2 = solve (uncurry (<>)) True

    solve selectAntinodes addAntennaCoords (rowCount, colCount, antennaCoords) =
      antennaCoords
        & concatMap
          (pairs >>> concatMap (uncurry (antinodes rowCount colCount) >>> selectAntinodes))
        & (if addAntennaCoords then concat antennaCoords & (<>) else id)
        & ordNub
        & filter (inBounds rowCount colCount)
        & length

    pairs idxs = [(i, j) | i <- idxs, j <- idxs, i /= j]

parseLines :: [String] -> Input
parseLines rows =
  ( length rows,
    length (head rows),
    rows
      & zip [0 ..]
      & concatMap
        ((\(i, row) -> zipWith (\j c -> ((i, j), c)) [0 ..] row) >>> filter ((/= '.') . snd))
      & sortOn snd
      & groupBy ((==) `on` snd)
      & map (map fst)
  )

antinodes :: Int -> Int -> Point -> Point -> ([Point], [Point])
antinodes rowCount colCount (x1, y1) (x2, y2) =
  let (ox, oy) = (x1 - x2, y1 - y2)
   in (go (ox, oy) (x1, y1), go (-ox, -oy) (x2, y2))
  where
    go (ox, oy) =
      iterateMaybe (\(x, y) -> ensure (inBounds rowCount colCount) (x + ox, y + oy)) >>> tail

inBounds :: Int -> Int -> Point -> Bool
inBounds rowCount colCount (x, y) = x >= 0 && x < rowCount && y >= 0 && y < colCount
