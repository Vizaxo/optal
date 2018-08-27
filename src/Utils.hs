module Utils where

import Control.Monad
import Debug.Trace

halves xs = let n = length xs `div` 2
            in (take n xs, drop n xs)

spy :: Show a => a -> a
spy x = traceShow x x

catchFailure :: e -> Maybe a -> Either e a
catchFailure _ (Just x) = Right x
catchFailure e Nothing = Left e

replaceFailureWith :: a -> Either e a -> a
replaceFailureWith _ (Right x) = x
replaceFailureWith x (Left _) = x

toMplus :: MonadPlus m => Maybe a -> m a
toMplus Nothing = mzero
toMplus (Just x) = pure x
