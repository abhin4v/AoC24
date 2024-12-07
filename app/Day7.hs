module Main where

import AoC qualified
import Data.Char (isDigit)

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ map parseLine . lines
  where
    parseLine s =
      let (res : args) = words s in (read $ takeWhile isDigit res, map read args)

    solvePart1 = solve [(+), (*)]
    solvePart2 = solve [(+), (*), conc]

    solve ops resArgs =
      sum [res | (res, args) <- resArgs, res `elem` eval ops [] args]

    eval _ ress [] = ress
    eval ops [] (a : as) = eval ops [a] as
    eval ops ress (a : as) = eval ops [res `op` a | res <- ress, op <- ops] as

    conc a b = read $ show a <> show b
