module Compiler where

import InteractionNet
import Term
import Utils

import Control.Lens
import Control.Monad.Writer
import Control.Monad.State
import Data.List

-- | Compile a term into an InteractionNet
compile :: Term -> InteractionNet
compile t = execWriter (evalStateT go 0)
  where go = do root <- fresh
                compile' (root, Principal) t

-- | Helper compile function, where the work gets done
compile'
  :: (MonadWriter InteractionNet m -- ^ Graph connections output
    ,MonadState Addr m)           -- ^ Next fresh address
  => NodePort                     -- ^ Root node to attach to
  -> Term                         -- ^ The term to compile
  -> m [FreeVar]                  -- ^ Returns the ports to connect variables to
compile' root (TVar i) = do
  pure [(root, i)]
compile' root (TLam body) = do
  lamNode <- insertNode NodLam
  insertConn (root, (lamNode, Tertiary))
  (toBind, frees) <- partition ((== 0) . snd) <$> compile' (lamNode, Secondary) body
  share (fst <$> toBind) >>= \case
    Nothing -> return ()
    Just var -> insertConn (var, (lamNode, Principal))
  pure (over (each . _2) (subtract 1) frees)
compile' root (TApp f x) = do
  appNode <- insertNode NodApp
  insertConn (root, (appNode, Tertiary))
  ffrees <- compile' (appNode, Principal) f
  xfrees <- compile' (appNode, Secondary) x
  pure (ffrees <> xfrees)

-- | Connect a list of nodes with the appropriate fan nodes for
-- sharing, and return the root of this shared structure.
share
  :: (MonadWriter InteractionNet m, MonadState Addr m)
  => [NodePort]         -- ^ The list of nodes to share
  -> m (Maybe NodePort) -- ^ Nothing if there are no nodes to share; Just the root otherwise
share [] = pure Nothing
share [x] = pure $ Just x
share xs = do
  let (as, bs) = halves xs
  share as >>= \case
    Nothing -> share bs
    Just a -> do
      share bs >>= \case
        Nothing -> pure Nothing
        Just b -> do
          fanNode <- insertNode NodFan
          insertConn (a, (fanNode, Secondary))
          insertConn (b, (fanNode, Tertiary))
          pure $ Just (fanNode, Principal)
