module AoC.Monad where

import Control.Monad (when)

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM mb m = do
  b <- mb
  when b (m >> whileM mb m)
{-# INLINE whileM #-}

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (pure ()) f =<< mg
{-# INLINE whenJustM #-}

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb t = ifM mb t (pure ())
{-# INLINE whenM #-}

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mb = ifM mb (pure ())
{-# INLINE unlessM #-}

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb t f = do
  b <- mb
  if b then t else f
{-# INLINE ifM #-}

maybeM :: (Monad m) => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM mn f mx = maybe mn f =<< mx
{-# INLINE maybeM #-}

whenMaybe :: (Applicative m) => Bool -> m a -> m (Maybe a)
whenMaybe b mx = if b then Just <$> mx else pure Nothing
{-# INLINE whenMaybe #-}

iterateMaybeM :: (Monad m) => (a -> m (Maybe a)) -> a -> m [a]
iterateMaybeM f x =
  f x >>= \case
    Nothing -> return [x]
    Just y -> (x :) <$> iterateMaybeM f y
