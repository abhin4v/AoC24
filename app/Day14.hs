module Main where

import AoC qualified
import Control.Arrow ((>>>))
import Data.Bifunctor (bimap)
import Data.List (minimumBy)
import Data.List.HT (chop)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Tuple.HT (snd3, thd3)
import Linear (V2 (..))

data Robot = Robot !(V2 Int) !(V2 Int) deriving (Show)

main :: IO ()
main = AoC.main solvePart1 solvePart2 parse
  where
    (width, height) = (101, 103)
    solvePart1 = map (iterate move >>> (!! 100)) >>> calcSafetyFactor
    solvePart2 =
      (,0)
        >>> iterate (bimap (map move) (+ 1))
        >>> take (width * height + 1)
        >>> map (\(robots, seconds) -> (robots, seconds, calcSafetyFactor robots))
        >>> minimumBy (comparing thd3)
        >>> snd3

    parse = lines >>> map (chop (== ' ') >>> map parsePoint >>> (\[p, v] -> Robot p v))
      where
        parsePoint =
          chop (== '=') >>> tail >>> head >>> chop (== ',') >>> map read >>> (\[x, y] -> V2 x y)

    move (Robot (V2 px py) vel@(V2 vx vy)) =
      Robot (V2 ((px + vx + width) `rem` width) ((py + vy + height) `rem` height)) vel

    calcSafetyFactor =
      foldl (\m (Robot pos _) -> Map.insertWith (+) pos 1 m) Map.empty
        >>> Map.mapKeysWith (+) (quadrant (width `quot` 2) (height `quot` 2))
        >>> Map.delete 0
        >>> Map.elems
        >>> product

    quadrant halfWidth halfHeight (V2 x y)
      | x < halfWidth && y < halfHeight = 1
      | x < halfWidth && y >= halfHeight + 1 = 2
      | x >= halfWidth + 1 && y >= halfHeight + 1 = 3
      | x >= halfWidth + 1 && y < halfHeight = 4
      | otherwise = 0

-- main = readFile "inputs/Day14.txt" >>= simulateAndRender 101 103 0 1000000000 . parse

-- simulateAndRender :: Int -> Int -> Int -> Int -> [Robot] -> IO ()
-- simulateAndRender width height seconds minSF robots = do
--   let sf = calcSafetyFactor width height robots
--   print (seconds, sf, minSF)
--   render width height (foldl (\m (Robot pos _) -> Map.insertWith (+) pos 1 m) Map.empty robots)
--   threadDelay 1000
--   when (sf <= minSF) $ void getChar
--   when (seconds <= width * height) $
--     simulateAndRender width height (seconds + 1) (min sf minSF) (map (move width height) robots)
--   where
--     render width height robotPoss = do
--       forM_ [0 .. width - 1] $ \i -> do
--         forM_ [0 .. height - 1] $ \j ->
--           putStr $ if V2 j i `Map.member` robotPoss then "██" else "  "
--         putStrLn ""
--       putStrLn ""
