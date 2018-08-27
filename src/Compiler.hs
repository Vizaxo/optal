module Compiler where

import InteractionNet
import Term
import Utils

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List

-- | Compile a term into an InteractionNet
compile :: Term -> InteractionNet
compile t = execWriter (evalStateT (runReaderT go 0) 0)
  where go = do root <- fresh
                compile' (root, Principal) t

-- | Helper compile function, where the work gets done
compile'
  :: (MonadReader Int m            -- ^ Current index
    ,MonadWriter InteractionNet m -- ^ Graph connections output
    ,MonadState Addr m            -- ^ Next fresh address
    )
  => NodePort                     -- ^ Root node to attach to
  -> Term                         -- ^ The term to compile
  -> m [FreeVar]                  -- ^ Returns the ports to connect variables to
compile' root (TVar i) = do
  index <- ask
  croi <- insertNode (Node NodCroi index)
  insertConn (root, (croi, Secondary))
  pure [((croi, Principal), i)]
compile' root (TLam body) = do
  index <- ask
  lamNode <- insertNode (Node NodLam index)
  insertConn (root, (lamNode, Principal))
  (toBind, frees) <- partition ((== 0) . snd) <$> compile' (lamNode, Secondary) body
  share (fst <$> toBind) >>= \case
    Nothing -> return ()
    Just var -> insertConn (var, (lamNode, Tertiary))
  pure (over (each . _2) (subtract 1) frees)
compile' root (TApp f x) = do
  index <- ask
  appNode <- insertNode (Node NodApp index)
  insertConn (root, (appNode, Tertiary))
  ffrees <- compile' (appNode, Principal) f
  xfreesNoBracket <- local (+1) $ compile' (appNode, Secondary) x
  xfrees <- traverseOf (each . _1) addBracket xfreesNoBracket
  pure (ffrees <> xfrees)

addBracket
  :: (MonadReader Int m            -- ^ Current index
    ,MonadWriter InteractionNet m -- ^ Graph connections output
    ,MonadState Addr m            -- ^ Next fresh address
    )
  => NodePort   -- ^ The port to add the bracket to
  -> m NodePort -- ^ The new port that replaces the input port
addBracket port = do
  index <- ask
  bracket <- insertNode (Node NodBrac index)
  insertConn (port, (bracket, Secondary))
  pure (bracket, Principal)

-- | Connect a list of nodes with the appropriate fan nodes for
-- sharing, and return the root of this shared structure.
share
  :: (MonadReader Int m            -- ^ Current index
    ,MonadWriter InteractionNet m -- ^ Graph connections output
    ,MonadState Addr m            -- ^ Next fresh address
    )
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
          index <- ask
          fanNode <- insertNode (Node NodFan index)
          insertConn (a, (fanNode, Secondary))
          insertConn (b, (fanNode, Tertiary))
          pure $ Just (fanNode, Principal)
