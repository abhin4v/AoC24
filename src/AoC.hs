module AoC (main) where

import Data.Time (diffUTCTime, getCurrentTime)
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
        "0" -> do
          printSolution $ solvePart1 input
          printSolution $ solvePart2 input
        _ -> IO.hPutStrLn IO.stderr "Expected 0 or 1 or 2 for part" >> exitFailure
    _ -> IO.hPutStrLn IO.stderr "Usage: <exe> <0 or 1 or 2> <input_file>" >> exitFailure
  where
    printSolution s = do
      start <- getCurrentTime
      putStrLn $ "=== " <> show s <> " ==="
      elapsed <- flip diffUTCTime start <$> getCurrentTime
      putStrLn $ "(" <> show elapsed <> ")"
