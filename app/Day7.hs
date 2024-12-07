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
      sum [res | (res, args) <- resArgs, res `elem` eval ops res [] args]

    eval _ _ ress [] = ress
    eval ops finalRes [] (a : as) = eval ops finalRes [a] as
    eval ops finalRes ress (a : as) =
      flip (eval ops finalRes) as $
        [res' | res <- ress, op <- ops, let res' = res `op` a, res' <= finalRes]

    conc a b = read $ show a <> show b
