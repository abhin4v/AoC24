module Main where

import AoC qualified
import Data.Foldable (foldl')
import Data.List (isPrefixOf)
import Data.List.HT (chop, dropRev)
import Text.Regex.TDFA (getAllTextMatches, (=~))

data Instr = Mul Int Int | Do | Dont deriving (Show)

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ \input ->
  map parseInstr
    . getAllTextMatches
    $ input =~ ("(mul\\([0-9]{1,3},[0-9]{1,3}\\))|(do\\(\\))|(don't\\(\\))" :: String)
  where
    solvePart1 instrs = sum [n1 * n2 | Mul n1 n2 <- instrs]
    solvePart2 = fst . foldl' processInstr (0, True)

    parseInstr = \case
      "do()" -> Do
      "don't()" -> Dont
      instr
        | "mul" `isPrefixOf` instr ->
            (\[n1, n2] -> Mul n1 n2) . map read . chop (== ',') . dropRev 1 . drop 4 $ instr
      instr -> error $ "Bad instruction: " <> instr

    processInstr (sum, enabled) = \case
      Do -> (sum, True)
      Dont -> (sum, False)
      Mul n1 n2 | enabled -> (sum + n1 * n2, enabled)
      _ -> (sum, enabled)
