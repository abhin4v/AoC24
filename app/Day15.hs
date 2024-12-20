module Main where

import AoC qualified
import AoC.Monad (iterateMaybeM)
import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (foldM_)
import Control.Monad.ST.Strict (runST)
import Data.Bifunctor (bimap)
import Data.List qualified as L
import Data.List.HT (breakAfter, chop)
import Data.Massiv.Array (Ix2 (..))
import Data.Massiv.Array qualified as A
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import System.Console.ANSI qualified as C
import System.IO qualified as IO

data Move = U | D | L | R deriving (Show)

type Grid = A.Array A.P Ix2 Char

type MGrid m = A.MArray (A.PrimState m) A.P Ix2 Char

type Input = (Grid, [Move])

main :: IO ()
main = AoC.main solvePart1 solvePart2 parse

-- main = readFile "inputs/Day15.txt" >>= simulateAndRender . parse

parse :: String -> Input
parse =
  lines >>> chop (== "") >>> (\[g, m] -> (g, m)) >>> bimap parseGrid (concat >>> map parseMove)
  where
    parseGrid = A.fromLists' A.Seq
    parseMove = \case '^' -> U; 'v' -> D; '<' -> L; '>' -> R; c -> error $ "Invalid: " <> show c

solvePart1 :: Input -> Int
solvePart1 (grid, moves) = runST $ do
  mGrid <- A.thawS grid
  foldM_ (step mGrid) (robotPos grid) moves
  (boxGPSCoords >>> sum) <$> A.freezeS mGrid

step :: (A.PrimMonad m) => MGrid m -> Ix2 -> Move -> m Ix2
step grid !robotIx move = do
  v <- vista grid robotIx move
  robotIx' <- case head v of
    (ix, '.') -> return ix
    (_, '#') -> return robotIx
    (ix, 'O') -> case L.find ((== '.') . snd) v of
      Nothing -> return robotIx
      Just (ix', _) -> A.swap_ grid ix ix' >> return ix
    (_, c) -> error $ "Invalid char: " <> show c
  A.swap_ grid robotIx robotIx'
  return robotIx'

vista :: (A.PrimMonad m) => MGrid m -> Ix2 -> Move -> m [(Ix2, Char)]
vista grid ix move = do
  tail . fst . breakAfter ((== '#') . snd) <$> iterateMaybeM (\(i, _) -> let i' = i + moveInc in fmap (i',) <$> A.read grid i') (ix, '@')
  where
    moveInc = case move of
      U -> (-1) :. 0
      D -> 1 :. 0
      L -> 0 :. -1
      R -> 0 :. 1

boxGPSCoords :: Grid -> [Int]
boxGPSCoords = A.ifoldMono $ \(x :. y) e -> if e == 'O' then [100 * x + y] else [0]

robotPos :: Grid -> Ix2
robotPos =
  A.ifoldMono (\ix e -> if e == '@' then First (Just ix) else First Nothing)
    >>> getFirst
    >>> fromJust

solvePart2 :: Input -> Int
solvePart2 = undefined

render :: MGrid IO -> IO ()
render mGrid = do
  A.freeze A.Par mGrid
    >>= (A.toLists2 >>> map (concatMap renderChar) >>> L.intercalate "\n" >>> putStrLn)
  where
    renderChar = \case
      '.' -> "   "
      'O' -> "📦 "
      '#' -> "🚧 "
      '@' -> "🤖 "
      c -> [c, ' ']

simulateAndRender :: Input -> IO ()
simulateAndRender (grid, moves) = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  (C.clearScreen >> C.hideCursor >> A.thaw grid >>= flip (go 0 $ robotPos grid) moves)
    `finally` C.showCursor
  where
    go steps ix mGrid = \case
      [] -> render mGrid
      (move : rest) -> do
        C.setCursorPosition 0 0
        render mGrid
        ix' <- step mGrid ix move
        threadDelay $
          if (steps `mod` 1000) < 30 then 100000 else 2000
        go (steps + 1) ix' mGrid rest
