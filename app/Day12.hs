module Main where

import AoC qualified
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Graph.Wrapper qualified as Graph
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Massiv.Array qualified as A

main :: IO ()
main =
  AoC.main (solve perimeterStencil) (solve sideCountStencil) $ lines >>> A.fromLists' @A.P A.Seq

solve :: A.Stencil Ix2 Char (Char, Int, [Ix2]) -> A.Array A.P Ix2 Char -> Int
solve stencil =
  A.mapStencil @_ @A.P (A.Fill '*') stencil
    >>> A.computeP @A.B
    >>> A.imap (\i (t, p, ns) -> (t, p, i, map (+ i) ns))
    >>> A.toList
    >>> foldl' (\m (t, p, i, ns) -> Map.insertWith (<>) t [(i, (ns, p))] m) Map.empty
    >>> Map.map (Map.fromList >>> calcPrice)
    >>> sum

perimeterStencil :: A.Stencil Ix2 Char (Char, Int, [Ix2])
perimeterStencil = makeStencil $ \get ->
  sum $ map (\p -> if get p == get (0 :. 0) then 0 else 1) [-1 :. 0, 0 :. 1, 1 :. 0, 0 :. -1]

sideCountStencil :: A.Stencil Ix2 Char (Char, Int, [Ix2])
sideCountStencil = makeStencil $ \get ->
  map (map get) corners
    & filter (flip any [isConcaveCorner, isConvexCorner] . flip ($ get (0 :. 0)))
    & length
  where
    corners =
      [ [0 :. -1, -1 :. -1, -1 :. 0],
        [-1 :. 0, -1 :. 1, 0 :. 1],
        [0 :. 1, 1 :. 1, 1 :. 0],
        [1 :. 0, 1 :. -1, 0 :. -1]
      ]

    isConcaveCorner pt ~[c1, _, c3] = c1 /= pt && c3 /= pt
    isConvexCorner pt ~[c1, c2, c3] = c1 == pt && c3 == pt && c2 /= pt

makeStencil :: ((Ix2 -> Char) -> Int) -> A.Stencil Ix2 Char (Char, Int, [Ix2])
makeStencil getMetric = A.makeStencil (Sz (3 :. 3)) (1 :. 1) $ \get ->
  let plantType = get (0 :. 0)
      sameTypePoints = filter (\p -> get p == plantType) [-1 :. 0, 0 :. 1, 1 :. 0, 0 :. -1]
   in (plantType, getMetric get, sameTypePoints)

calcPrice :: Map.Map Ix2 ([Ix2], Int) -> Int
calcPrice neighboursAndPerimeters =
  Map.map fst neighboursAndPerimeters
    & Map.assocs
    & Graph.fromListSimple
    & Graph.stronglyConnectedComponents
    & map
      ( \case
          Graph.AcyclicSCC i -> snd $ neighboursAndPerimeters Map.! i
          Graph.CyclicSCC is -> is & map ((neighboursAndPerimeters Map.!) >>> snd) & sum & (* length is)
      )
    & sum
