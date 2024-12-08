module Main where

import AoC qualified
import Control.Arrow ((>>>))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.IntSet qualified as Set
import Data.List (sortBy)
import Data.List.HT (breakAfter, chop)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = AoC.main solvePart1 solvePart2 parse
  where
    parse =
      lines
        >>> breakAfter (== "")
        >>> bimap
          ( init
              >>> map (breakAfter (== '|') >>> bimap (read . init) read)
              >>> foldl' (\m (p1, p2) -> Map.insertWith Set.union p1 (Set.singleton p2) m) Map.empty
          )
          (map (chop (== ',') >>> map read))

    solvePart1 = solve id (const id)

    solvePart2 = solve not $ \rules -> sortBy $ \p1 p2 ->
      if
        | maybe False (p2 `Set.member`) $ Map.lookup p1 rules -> LT
        | maybe False (p1 `Set.member`) $ Map.lookup p2 rules -> GT
        | otherwise -> EQ

    solve updatePred sortUpdates (rules, updates) =
      updates
        & filter (updatePred . isCorrectUpdate rules)
        & map ((\l -> l !! (length l `quot` 2)) . sortUpdates rules)
        & sum

    isCorrectUpdate rules = go Set.empty
      where
        go prev = \case
          [] -> True
          (page : rest) ->
            let after = Set.toList $ fromMaybe Set.empty $ Map.lookup page rules
             in not (any (`Set.member` prev) after) && go (Set.insert page prev) rest
