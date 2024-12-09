module Main where

import AoC qualified
import AoC.Monad (unlessM, whenJustM, whileM)
import Control.Arrow ((>>>))
import Control.Monad (when)
import Control.Monad.ST.Strict (ST, runST)
import Data.Char (digitToInt)
import Data.IntSet qualified as Set
import Data.STRef.Strict (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Vector.Primitive qualified as V
import Data.Vector.Primitive.Mutable qualified as MV

type DiskMap = V.Vector Int

type MDiskMap s = MV.MVector s Int

type IndexRef s = STRef s Int

main :: IO ()
main = AoC.main solvePart1 solvePart2 parse
  where
    solvePart1 = compactBlocks >>> checksum
    solvePart2 = compactFiles >>> checksum
    checksum = V.ifoldl' (\a i x -> if x /= -1 then a + i * x else a) 0

parse :: String -> DiskMap
parse =
  lines
    >>> head
    >>> zipWith (\i s -> replicate (digitToInt s) (if even i then i `quot` 2 else -1)) [0 ..]
    >>> concat
    >>> V.fromList

compactBlocks :: DiskMap -> DiskMap
compactBlocks input = runST $ do
  freeIdxRef <- newSTRef 0
  blockIdxRef <- newSTRef $ V.length input - 1
  diskMap <- V.thaw input

  seekForwardToStartOfFreeSpace diskMap freeIdxRef
  seekBackwardToEndOfFile diskMap blockIdxRef

  whileM ((<) <$> readIdx freeIdxRef <*> readIdx blockIdxRef) $ do
    freeIdx <- readIdx freeIdxRef
    blockIdx <- readIdx blockIdxRef
    when (freeIdx < MV.length diskMap && blockIdx > 0) $ do
      MV.unsafeSwap diskMap freeIdx blockIdx
      seekForwardToStartOfFreeSpace diskMap freeIdxRef
      seekBackwardToEndOfFile diskMap blockIdxRef

  V.freeze diskMap

compactFiles :: DiskMap -> DiskMap
compactFiles input = runST $ do
  firstFreeIdxRef <- newSTRef 0
  blockIdxRef <- newSTRef $ V.length input - 1
  movedFilesRef <- newSTRef Set.empty
  doneRef <- newSTRef False
  diskMap <- V.thaw input

  let isNotOver = do
        blockIdx <- readIdx blockIdxRef
        done <- readSTRef doneRef
        return $ not done && blockIdx > 0

  whileM isNotOver $ do
    seekBackwardToEndOfFile diskMap blockIdxRef
    whenJustM (getFileAtBlockIdxAndSeek diskMap blockIdxRef) $
      \(fileId, fileStartIdx, fileSize) -> do
        firstFreeIdx <- readIdx firstFreeIdxRef
        when (fileStartIdx < firstFreeIdx) $ writeSTRef doneRef True
        unlessM ((fileId `Set.member`) <$> readSTRef movedFilesRef) $ do
          whenJustM (findFreeSpace diskMap firstFreeIdx fileStartIdx fileSize) $
            \(freeIdx, firstFreeIdx) -> do
              fill diskMap fileStartIdx fileSize (-1)
              fill diskMap freeIdx fileSize fileId
              writeIdx firstFreeIdxRef firstFreeIdx
              modifySTRef' movedFilesRef $ Set.insert fileId

  V.freeze diskMap

getFileAtBlockIdxAndSeek :: MDiskMap s -> IndexRef s -> ST s (Maybe (Int, Int, Int))
getFileAtBlockIdxAndSeek diskMap blockIdxRef =
  readDiskMap diskMap blockIdxRef >>= \case
    Nothing -> return Nothing
    Just fileId -> do
      fileEndIdx <- readIdx blockIdxRef
      seekBackwardToStartOfFile fileId diskMap blockIdxRef
      fileStartIdx <- (+ 1) <$> readIdx blockIdxRef
      return $ Just (fileId, fileStartIdx, fileEndIdx - fileStartIdx + 1)
{-# INLINE getFileAtBlockIdxAndSeek #-}

findFreeSpace :: MDiskMap s -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
findFreeSpace diskMap startIdx endIdx size = do
  let diskSize = MV.length diskMap
  freeIdxRef <- newSTRef startIdx
  let scan = do
        freeIdx <- readIdx freeIdxRef
        if freeIdx >= diskSize || freeIdx >= endIdx
          then return Nothing
          else do
            startIdx <- readIdx freeIdxRef
            seekForwardToEndOfFreeSpace diskMap freeIdxRef
            endIdx <- readIdx freeIdxRef
            if endIdx < diskSize && endIdx - startIdx >= size
              then return $ Just freeIdx
              else seekForwardToStartOfFreeSpace diskMap freeIdxRef >> scan

  seekForwardToStartOfFreeSpace diskMap freeIdxRef
  firstFreeIdx <- readIdx freeIdxRef
  fmap (,firstFreeIdx) <$> scan
{-# INLINE findFreeSpace #-}

fill :: MDiskMap s -> Int -> Int -> Int -> ST s ()
fill diskMap startIdx size = MV.set (MV.slice startIdx size diskMap)
{-# INLINE fill #-}

seekForwardToStartOfFreeSpace :: MDiskMap s -> IndexRef s -> ST s ()
seekForwardToStartOfFreeSpace = seekWhile (/= -1) (+ 1)

seekForwardToEndOfFreeSpace :: MDiskMap s -> IndexRef s -> ST s ()
seekForwardToEndOfFreeSpace = seekWhile (== -1) (+ 1)

seekBackwardToStartOfFile :: Int -> MDiskMap s -> IndexRef s -> ST s ()
seekBackwardToStartOfFile fileId = seekWhile (== fileId) (subtract 1)

seekBackwardToEndOfFile :: MDiskMap s -> IndexRef s -> ST s ()
seekBackwardToEndOfFile = seekWhile (== -1) (subtract 1)

seekWhile :: (Int -> Bool) -> (Int -> Int) -> MDiskMap s -> IndexRef s -> ST s ()
seekWhile prd inc diskMap idx =
  whileM ((\case Nothing -> False; Just x -> prd x) <$> readDiskMap diskMap idx) $
    modifySTRef' idx inc

readDiskMap :: MDiskMap s -> IndexRef s -> ST s (Maybe Int)
readDiskMap diskMap idxRef = do
  idx <- readIdx idxRef
  if idx < MV.length diskMap
    then Just <$> MV.unsafeRead diskMap idx
    else return Nothing

readIdx :: IndexRef s -> ST s Int
readIdx = readSTRef

writeIdx :: IndexRef s -> Int -> ST s ()
writeIdx = writeSTRef
