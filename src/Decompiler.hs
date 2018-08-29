module Decompiler where

import InteractionNet
import Term

import Control.Lens hiding (Index)
import Control.Monad.Reader
import Data.List
import qualified Data.Map as M

-- | Decompile an 'InteractionNet' back into a 'Term'. This can fail
-- if the net is not connected together correctly.
decompile :: InteractionNet -> Maybe Term
decompile = runReaderT (decompile' [] =<< getConnectedTo rootNode)

-- | Helper function for decompile.
decompile'
  :: (MonadReader InteractionNet m, MonadPlus m)
  => [(NodePort,Index)] -- ^ A list of the ports associated with
                        -- lambda-bound variables, and the De Bruijn
                        -- index of that variable
  -> NodePort           -- ^ The root of the part of the graph to decompile
  -> m Term             -- ^ The decompiled term
decompile' vars root@(raddr,_) =
  case elemIndex root (fst <$> vars) of
    Just i -> pure $ TVar i
    Nothing ->
      lookupNode raddr >>= \case
        Node NodLam  _ ->
          -- Increment the De Bruijn indices, and add this lambda's
          -- variable to the list
          let vars' = ((raddr, Tertiary), 0):over (each . _2) (+1) vars
          in TLam <$> (decompile' vars' =<< getConnectedTo (raddr, Secondary))
        Node NodApp  _ -> do
          f <- decompile' vars =<< getConnectedTo (raddr, Principal)
          x <- decompile' vars =<< getConnectedTo (raddr, Secondary)
          pure $ TApp f x
        Node NodCroi _ ->
          decompile' vars =<< getConnectedTo (raddr, Principal)
        Node NodBrac _ ->
          decompile' vars =<< getConnectedTo (raddr, Principal)
        Node NodFan  _ ->
          error "Not implemented yet"
