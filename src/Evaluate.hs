module Evaluate where

import InteractionNet

eval :: InteractionNet -> InteractionNet
eval = undefined



evalConn :: Connection -> InteractionNet -> Maybe InteractionNet
evalConn ((a, Principal), (b, Principal)) net
  = do a' <- lookupNode a net
       b' <- lookupNode b net
       runRule a b net a' b'

runRule :: Addr -> Addr -> InteractionNet -> Node -> Node -> Maybe InteractionNet
runRule a b net NodApp NodLam = betaReduce a b net
runRule a b net NodLam NodApp = runRule b a net NodApp NodLam
runRule _ _ _   _      _      = Nothing

betaReduce :: Addr -> Addr -> InteractionNet -> Maybe InteractionNet
betaReduce a b net = Nothing
  -- Remove app,lambda node (not considering sharing for now)
  -- Replace conn of root to app with root to lam
  -- Remove conn of var to lam
  -- Remove conn of app to x
  -- Add conn of x to var
