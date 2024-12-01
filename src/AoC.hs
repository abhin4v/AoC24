module AoC (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO qualified as IO

main :: (Show a1, Show a2) => (t -> a1) -> (t -> a2) -> (String -> t) -> IO ()
main solvePart1 solvePart2 parseInput =
  getArgs >>= \case
    [part, inputFile] -> do
      input <- parseInput <$> readFile inputFile
      case part of
        "1" -> printSolution $ solvePart1 input
        "2" -> printSolution $ solvePart2 input
        _ -> IO.hPutStrLn IO.stderr "Expected 1 or 2 for part" >> exitFailure
    _ -> IO.hPutStrLn IO.stderr "Usage: <exe> <1 or 2> <input_file>" >> exitFailure
  where
    printSolution s = putStrLn $ "=== " <> show s <> " ==="
