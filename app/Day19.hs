module Main where

import AoC qualified
import Control.Arrow ((>>>))
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.MemoTrie (memoFix)
import Data.Trie qualified as Trie
import Data.Tuple.HT (thd3)

main :: IO ()
main = AoC.main solvePart1 solvePart2 $ \input ->
  let (stripes : _ : patterns) = lines input
   in (BS.pack stripes & BS.split ',' & map (BS.strip >>> (,())) & Trie.fromList, patterns)
  where
    solvePart1 (t, patterns) = patterns & filter (countCombs t >>> (/= 0)) & length
    solvePart2 (t, patterns) = patterns & map (countCombs t) & sum
    countCombs t = memoFix $ \f pattern -> case pattern of
      [] -> 1
      _ -> pattern & BS.pack & Trie.matches t & map (thd3 >>> BS.unpack >>> f) & sum
