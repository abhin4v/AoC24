module Main where

import AoC qualified
import AoC.List (ensure)
import Data.Char (isDigit)
import Data.List (isSuffixOf)

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ map parseLine . lines
  where
    parseLine s =
      let (res : args) = words s in (read $ takeWhile isDigit res, map read args)

    solvePart1 = solve [mSub, mDiv]
    solvePart2 = solve [mSub, mDiv, mSplit]

    solve ops resArgs =
      sum [res | (res, first : rest) <- resArgs, first `elem` eval ops first [res] (reverse rest)]

    eval _ _ ress [] = ress
    eval ops first ress (a : as) =
      flip (eval ops first) as $
        [res' | res <- ress, op <- ops, Just res' <- [res `op` a], res' >= first]

    mSub a b = ensure (> 0) $ a - b
    mDiv a b = let (q, r) = a `quotRem` b in if r == 0 then Just q else Nothing
    mSplit a b =
      let len = length (show a) - length (show b)
       in if show b `isSuffixOf` show a && len > 0
            then Just $ read $ take len $ show a
            else Nothing
