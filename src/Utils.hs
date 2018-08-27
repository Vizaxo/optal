module Utils where

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
