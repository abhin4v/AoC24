module Main where

import AoC qualified
import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (void)
import Data.Bits ((.&.), (.|.))
import Data.List qualified as L
import Data.Massiv.Array (Ix2 (..), Sz (..))
import Data.Massiv.Array qualified as A
import Data.Massiv.Array.Mutable qualified as AM
import Data.Massiv.Array.Mutable.Algorithms qualified as AM
import Data.Maybe (isJust)
import Data.Monoid (Sum (Sum, getSum))
import Data.Word (Word8)
import System.Console.ANSI qualified as C
import System.IO qualified as IO

type Input = A.Array A.P Ix2 Char

main :: IO ()
main = AoC.main solvePart1 solvePart2 parse

-- main = readFile "inputs/Day6.txt" >>= simulateAndRender . parse

parse :: String -> Input
parse = lines >>> A.fromLists' @A.P A.Seq

solvePart1 :: Input -> Int
solvePart1 =
  getSum
    . A.foldMono (\case 'X' -> Sum 1; _ -> Sum 0)
    . simulate

solvePart2 :: Input -> Int
solvePart2 =
  possibleObstacles
    >>> map simulate
    >>> filter endsInLoop
    >>> length
  where
    possibleObstacles input =
      [ snd $ A.withMArrayST input (\ma -> A.write ma i '#')
        | (i, c) <- A.toList $ A.imap (,) $ simulate input,
          c == 'X'
      ]

    endsInLoop = isJust . A.findIndex (== '*')

simulate :: Input -> Input
simulate =
  A.map (,0)
    >>> A.compute @A.U
    >>> A.iterateUntil (\_ prev cur -> A.map fst prev == A.map fst cur) (const step)
    >>> A.map fst
    >>> A.compute @A.P

-- obstacle: #
-- guard: ^><v
-- boundary: @
-- not visted: .
-- visited: X

type Grid = A.Array A.U Ix2 (Char, Word8)

step :: Grid -> Grid
step = A.computeP @A.U . A.mapStencil (A.Fill ('@', 0)) guardMovement
  where
    guardMovement = A.makeStencil (Sz (3 :. 3)) (1 :. 1) $ \get ->
      let [north, east, south, west] = map (fst . get) [-1 :. 0, 0 :. 1, 1 :. 0, 0 :. -1]
       in case get (0 :. 0) of
            (c, movements) | c `elem` "#*" -> (c, movements)
            (c, movements) | c `elem` "@.X" -> (,movements) $ case (north, south, east, west) of
              ('v', _, _, _) -> 'v'
              (_, '^', _, _) -> '^'
              (_, _, '<', _) -> '<'
              (_, _, _, '>') -> '>'
              _ -> c
            ('^', movements) ->
              ( case north of '#' -> '>'; _ -> markVisited 0b0001 movements,
                movements .|. 0b0001 -- N: 1
              )
            ('v', movements) ->
              ( case south of '#' -> '<'; _ -> markVisited 0b0010 movements,
                movements .|. 0b0010 -- S: 2
              )
            ('>', movements) ->
              ( case east of '#' -> 'v'; _ -> markVisited 0b0100 movements,
                movements .|. 0b0100 -- E: 4
              )
            ('<', movements) ->
              ( case west of '#' -> '^'; _ -> markVisited 0b1000 movements,
                movements .|. 0b1000 -- W: 8
              )
            c -> error $ "Invalid char: " <> show c
    {-# INLINE guardMovement #-}

    markVisited mask movements = if mask .&. movements == mask then '*' else 'X'
    {-# INLINE markVisited #-}

render :: Input -> IO ()
render =
  A.toLists2
    >>> map (concatMap (replicate 2 . renderChar))
    >>> L.intercalate "\n"
    >>> putStrLn
  where
    renderChar = \case
      '>' -> '⯈'
      '<' -> '⯇'
      'v' -> '⯆'
      '^' -> '⯅'
      '.' -> ' '
      '#' -> '▓'
      'X' -> '█'
      c -> c

simulateAndRender :: Input -> IO ()
simulateAndRender input = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  (C.clearScreen >> C.hideCursor >> go (A.compute @A.U $ A.map (,0) input))
    `finally` (C.clearScreen >> C.showCursor)
  where
    go =
      void
        . AM.iterateUntilM
          (\_ prev cur -> (== prev) <$> AM.freeze A.Seq cur)
          ( \st g -> do
              C.setCursorPosition 0 0
              render $ A.compute $ A.map fst g
              threadDelay 1_000
              pure $ step g
          )
