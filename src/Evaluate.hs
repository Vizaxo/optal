module Evaluate where

import InteractionNet
import Utils

import Control.Monad.Except
import Control.Monad.Reader
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
  = iterateMb (foldM (\n conn -> runReaderT (evalConn conn) n) net conns)
  where
    iterateMb mx =
      do x <- mx
         if net == x
           then pure net
           else eval' x

-- | Evaluate a given connection, reducing it if any rules apply to it
evalConn
  :: (MonadReader InteractionNet m, MonadState Addr m, MonadPlus m)
  => Connection
  -> m InteractionNet
evalConn ((a, Principal), (b, Principal))
  = do a' <- lookupNode a
       b' <- lookupNode b
       runExceptT (runRule a b a' b') >>= \case
         Right x -> pure x
         Left () -> runExceptT (runRule b a b' a') >>= \case
           Right x -> pure x
           Left () -> ask
evalConn _ = ask

-- | Run the appropirate rule for the given pair of nodes
runRule
  :: (MonadReader InteractionNet m, MonadState Addr m, MonadError () m, MonadPlus m)
  => Addr -> Addr
  -> Node -> Node
  -> m InteractionNet
runRule a b (Node NodApp i) (Node NodLam j) | i == j
  = betaReduce a b
runRule a b (Node NodBrac i) (Node NodBrac j) | i == j
  = unaryPairAnhiliation a b
runRule a b (Node NodCroi i) (Node NodCroi j) | i == j
  = unaryPairAnhiliation a b
runRule a b (Node NodFan i) (Node NodFan j) | i == j
  = fanAnhiliation a b
runRule _ _ _ _
  = throwError ()

-- | Helper function for making an interaction rule
mkRule
  :: (MonadReader InteractionNet m, MonadState Addr m)
  => [Connection] -- ^ Insert these connections
  -> [Node]       -- ^ Insert these nodes
  -> [Connection] -- ^ Remove these connections
  -> [Addr]       -- ^ Remove these nodes
  -> m InteractionNet
mkRule insCons insNods remCons remNods
  = do net <- ask
       net' <- foldrM insNode net insNods
       pure $
         foldr insConn
         (foldr removeConn
          (foldr removeNode net' remNods) remCons) insCons

-- | Run a beta-reduction rule
betaReduce
  :: (MonadReader InteractionNet m, MonadState Addr m, MonadPlus m)
  => Addr -- ^ The application node
  -> Addr -- ^ The lambda node
  -> m InteractionNet -- ^ Nothing if the expected connections don't exist
betaReduce app lam = do
  rootToApp <- lookupConn (app, Tertiary)
  root <- getConnectedTo app rootToApp
  lamToBody <- lookupConn (lam, Secondary)
  body <- getConnectedTo lam lamToBody
  let rootToBody = (root, body)
  varToLam <- lookupConn (lam, Tertiary)
  var <- getConnectedTo lam varToLam
  appToX <- lookupConn (app, Secondary)
  x <- getConnectedTo app appToX
  let xToVar = (x, var)
  let appToLam = ((lam, Principal), (app, Principal))
  mkRule
    [xToVar, rootToBody]
    []
    [appToX, lamToBody, varToLam, appToLam, rootToApp]
    [lam, app]


-- | Eliminate a pair of brackets/croissants facing each other
unaryPairAnhiliation
  :: (MonadReader InteractionNet m, MonadState Addr m, MonadPlus m)
  => Addr
  -> Addr
  -> m InteractionNet
unaryPairAnhiliation a b = do
  rootToA <- lookupConn (a, Secondary)
  root <- getConnectedTo a rootToA
  aToB <- lookupConn (a, Principal)
  bToBody <- lookupConn (b, Secondary)
  body <- getConnectedTo b bToBody
  let rootToBody = (root, body)
  mkRule
    [rootToBody]
    []
    [rootToA, aToB, bToBody]
    [a, b]


-- | Eliminate a pair of fans facing each other
fanAnhiliation
  :: (MonadReader InteractionNet m, MonadState Addr m, MonadPlus m)
  => Addr
  -> Addr
  -> m InteractionNet
fanAnhiliation top bottom = do
  aToTop <- lookupConn (top, Secondary)
  bToTop <- lookupConn (top, Tertiary)
  cToBottom <- lookupConn (bottom, Secondary)
  dToBottom <- lookupConn (bottom, Tertiary)
  topToBottom <- lookupConn (top, Principal)
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
