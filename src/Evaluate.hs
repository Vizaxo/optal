module Evaluate where

import InteractionNet
import Utils

import Control.Monad
import Control.Monad.Fix

-- | Evaluate the net. Returns Nothing if an attempt to lookup a node fails.
eval :: InteractionNet -> Maybe InteractionNet
eval net@(InteractionNet heap conns)
  = iterateMb (foldM evalConn net conns)
  where
    iterateMb mx =
      do x <- mx
         if net == x
           then Just net
           else eval x

-- | Evaluate a given connection, reducing it if any rules apply to it
evalConn :: InteractionNet -> Connection -> Maybe InteractionNet
evalConn net ((a, Principal), (b, Principal))
  = do a' <- lookupNode a net
       b' <- lookupNode b net
       case runRule a b net a' b' of
         Right x -> x
         -- Try the rule the other way around
         Left () -> replaceFailureWith (Just net) (runRule b a net b' a')
evalConn net _ = Just net

-- | Run the appropirate rule for the given pair of nodes
runRule :: Addr -> Addr -> InteractionNet -> Node -> Node -> Either () (Maybe InteractionNet)
runRule a b net (Node NodApp i) (Node NodLam j) | i == j
  = Right $ betaReduce a b net
runRule _ _ _ _ _
  = Left ()

-- | Run a beta-reduction rule
betaReduce
  ::  Addr -- ^ The application node
  -> Addr -- ^ The lambda node
  -> InteractionNet
  -> Maybe InteractionNet -- ^ Nothing if the expected connections don't exist
betaReduce app lam net = do
  rootToApp <- lookupConn (app, Tertiary) net
  root <- getConnectedTo app rootToApp
  lamToBody <- lookupConn (lam, Secondary) net
  body <- getConnectedTo lam lamToBody
  let rootToBody = (root, body)
  varToLam <- lookupConn (lam, Tertiary) net
  var <- getConnectedTo lam varToLam
  appToX <- lookupConn (app, Secondary) net
  x <- getConnectedTo app appToX
  let xToVar = (x, var)
  let appToLam = ((lam, Principal), (app, Principal))
  pure
    -- Insert new connections
    $ insConn xToVar
    $ insConn rootToBody
    -- Clean up old connections
    $ removeConn appToX
    $ removeConn lamToBody
    $ removeConn varToLam
    $ removeConn appToLam
    $ removeConn rootToApp
    -- Clean up old nodes
    $ removeNode lam
    $ removeNode app
    $ net

-- | Eliminate a pair of brackets facing each other
bracketAnhiliation
  :: Addr
  -> Addr
  -> InteractionNet
  -> Maybe InteractionNet
bracketAnhiliation a b net = do
  rootToA <- lookupConn (a, Secondary) net
  root <- getConnectedTo a rootToA
  aToB <- lookupConn (a, Principal) net
  bToBody <- lookupConn (b, Secondary) net
  body <- getConnectedTo b bToBody
  let rootToBody = (root, body)
  pure
    -- Insert new connections
    $ insConn rootToBody
    -- Clean up old connections
    $ removeConn rootToA
    $ removeConn aToB
    $ removeConn bToBody
    -- Clean up old nodes
    $ removeNode a
    $ removeNode b
    $ net

-- | Eliminate a pair of croissants facing each other
croissantAnhiliation
  :: Addr
  -> Addr
  -> InteractionNet
  -> Maybe InteractionNet
croissantAnhiliation a b net = do
  rootToA <- lookupConn (a, Secondary) net
  root <- getConnectedTo a rootToA
  aToB <- lookupConn (a, Principal) net
  bToBody <- lookupConn (b, Secondary) net
  body <- getConnectedTo b bToBody
  let rootToBody = (root, body)
  pure
    -- Insert new connections
    $ insConn rootToBody
    -- Clean up old connections
    $ removeConn rootToA
    $ removeConn aToB
    $ removeConn bToBody
    -- Clean up old nodes
    $ removeNode a
    $ removeNode b
    $ net

-- | Eliminate a pair of pans facing each other
fanAnhiliation
  :: Addr
  -> Addr
  -> InteractionNet
  -> Maybe InteractionNet
fanAnhiliation top bottom net = do
  aToTop <- lookupConn (top, Secondary) net
  bToTop <- lookupConn (top, Tertiary) net
  cToBottom <- lookupConn (bottom, Secondary) net
  dToBottom <- lookupConn (bottom, Tertiary) net
  topToBottom <- lookupConn (top, Principal) net
  a <- getConnectedTo top aToTop
  b <- getConnectedTo top bToTop
  c <- getConnectedTo bottom cToBottom
  d <- getConnectedTo bottom dToBottom
  let aToC = (a, c)
  let bToD = (b, d)
  pure
    -- Insert new connections
    $ insConn aToC
    $ insConn bToD
    -- Clean up old connections
    $ removeConn aToTop
    $ removeConn bToTop
    $ removeConn cToBottom
    $ removeConn dToBottom
    $ removeConn topToBottom
    -- Clean up old nodes
    $ removeNode top
    $ removeNode bottom
    $ net

-- | Get the node on the other end of a connection to the target
getConnectedTo target ((adr1, prt1), (adr2, prt2))
  | adr1 == target = Just (adr2, prt2)
  | adr2 == target = Just (adr1, prt1)
  --TODO: return failure properly with an Either
  | otherwise = Nothing
