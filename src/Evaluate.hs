module Evaluate where

import InteractionNet
import Utils

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Foldable

-- | Evaluate the net. Returns Nothing if an attempt to lookup a node fails.
eval :: InteractionNet -> Maybe InteractionNet
eval net = flip evalState (highestAddr net + 1) $ runMaybeT $ eval' net

-- | Helper function for eval, to separate the logic from the handling
-- of the monads
eval' :: (MonadState Addr m, MonadPlus m) => InteractionNet -> m InteractionNet
eval' net@(InteractionNet heap conns)
  = iterateMb (foldM evalConn net conns)
  where
    iterateMb mx =
      do x <- mx
         if net == x
           then pure net
           else eval' x

-- | Evaluate a given connection, reducing it if any rules apply to it
evalConn
  :: (MonadState Addr m, MonadPlus m)
  => InteractionNet
  -> Connection
  -> m InteractionNet
evalConn net ((a, Principal), (b, Principal))
  = do a' <- toMplus $ lookupNode a net
       b' <- toMplus $ lookupNode b net
       if (index a' == index b')
         then runExceptT (runRule a b net a' b') >>= \case
           Right x -> pure x
           Left () -> runExceptT (runRule b a net b' a') >>= \case
             Right x -> pure x
             Left () -> pure net
         else pure net
evalConn net _ = pure net

-- | Run the appropirate rule for the given pair of nodes
runRule :: (MonadState Addr m, MonadError () m, MonadPlus m) => Addr -> Addr -> InteractionNet -> Node -> Node -> m InteractionNet
runRule a b net (Node NodApp i) (Node NodLam j) | i == j
  = betaReduce a b net
runRule _ _ _ _ _
  = throwError ()

-- | Helper function for making an interaction rule
mkRule
  :: MonadState Addr m
  => [Connection] -- ^ Insert these connections
  -> [Node]       -- ^ Insert these nodes
  -> [Connection] -- ^ Remove these connections
  -> [Addr]       -- ^ Remove these nodes
  -> InteractionNet
  -> m InteractionNet
mkRule insCons insNods remCons remNods net
  = do net' <- foldrM insNode net insNods
       pure $
         foldr insConn
         (foldr removeConn
          (foldr removeNode net' remNods) remCons) insCons

-- | Run a beta-reduction rule
betaReduce
  :: (MonadState Addr m, MonadPlus m)
  => Addr -- ^ The application node
  -> Addr -- ^ The lambda node
  -> InteractionNet
  -> m InteractionNet -- ^ Nothing if the expected connections don't exist
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
  mkRule
    [xToVar, rootToBody]
    []
    [appToX, lamToBody, varToLam, appToLam, rootToApp]
    [lam, app]
    net

-- | Eliminate a pair of brackets/croissants facing each other
unaryPairAnhiliation
  :: (MonadState Addr m, MonadPlus m)
  => Addr
  -> Addr
  -> InteractionNet
  -> m InteractionNet
unaryPairAnhiliation a b net = do
  rootToA <- lookupConn (a, Secondary) net
  root <- getConnectedTo a rootToA
  aToB <- lookupConn (a, Principal) net
  bToBody <- lookupConn (b, Secondary) net
  body <- getConnectedTo b bToBody
  let rootToBody = (root, body)
  mkRule
    [rootToBody]
    []
    [rootToA, aToB, bToBody]
    [a, b]
    net

-- | Eliminate a pair of fans facing each other
fanAnhiliation
  :: (MonadState Addr m, MonadPlus m)
  => Addr
  -> Addr
  -> InteractionNet
  -> m InteractionNet
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
  mkRule
    [aToC, bToD]
    []
    [aToTop, bToTop, cToBottom, dToBottom, topToBottom]
    [top, bottom]
    net
