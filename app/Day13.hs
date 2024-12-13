module Main where

import AoC qualified
import AoC.AStar (astar)
import Data.Maybe (mapMaybe)
import Data.Ratio (approxRational, denominator)
import Linear (V1 (..), V2 (..), inv22, transpose, (!*!))
import Numeric (lexDigits)
import Text.ParserCombinators.ReadP qualified as P

main :: IO ()
main = AoC.main solvePart1 solvePart2 parse
  where
    solvePart1 = sum . mapMaybe solveAStar
    solvePart2 =
      sum . mapMaybe (solveLinear . (\(Machine a b p) -> Machine a b (p + 10000000000000)))

data Machine = Machine !(V2 Integer) !(V2 Integer) !(V2 Integer)
  deriving (Show)

parse :: String -> [Machine]
parse input = case P.readP_to_S (P.sepBy1 machine newline <* P.eof) input of
  [(parsed, "")] -> parsed
  xs -> error $ "No parse: " <> show xs
  where
    machine = Machine <$> (move 'A' <* newline) <*> (move 'B' <* newline) <*> (prize <* newline)
    move c =
      V2
        <$> (P.string "Button " *> P.char c *> P.string ": X+" *> digits)
        <*> (P.string ", Y+" *> digits)
    prize = V2 <$> (P.string "Prize: X=" *> digits <* P.string ", Y=") <*> digits
    digits = read <$> P.readS_to_P lexDigits
    newline = P.char '\n'

solveAStar :: Machine -> Maybe Int
solveAStar (Machine a b prize) = fmap fst . flip (astar (V2 0 0) prize) (const 400) $ \cur ->
  if cur < prize then [(cur + a, 3), (cur + b, 1)] else []

solveLinear :: Machine -> Maybe Int
solveLinear (Machine a b prize) =
  let V2 (V1 aMoves) (V1 bMoves) =
        inv22 (transpose $ fmap fromIntegral <$> V2 a b) !*! transpose (V1 $ fromIntegral <$> prize)
   in if isPositiveInteger aMoves && isPositiveInteger bMoves
        then Just $ round $ 3 * aMoves + bMoves
        else Nothing
  where
    isPositiveInteger x = x > 0 && denominator (approxRational x 0.0001) == 1
