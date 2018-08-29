module InteractionNet where

import Term
import Utils

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple


-- | Nodes in the graph
data NodeType
  = NodLam  -- ^ Principal: root, Secondary: body, Tertiary: var
  | NodApp  -- ^ Principal: f. Secondary: x. Tertiary: root
  | NodFan  -- ^ Principal: single-end. Secondary: shared side a. Tertiary: shared side b
  | NodCroi -- ^ Principal: down. Secondary: up
  | NodBrac -- ^ Principal: down. Secondary: up
  deriving (Eq, Show)

-- | Each node has a type and an associated index
data Node
  = Node
    { nodeType :: NodeType
    , index :: Int
    }
  deriving (Eq, Show)

-- | Ports of graph nodes
data Port = Principal | Secondary | Tertiary
  deriving (Eq, Show)

-- | Addresses of nodes on the heap
type Addr = Int

-- | A global index to a node and port
type NodePort = (Addr, Port)

-- | Connection between two nodes
type Connection = (NodePort, NodePort)

-- | Adjacency list of the graph
type AdjList = [Connection]

-- | Heap containting all the nodes
type Heap = Map Addr Node

-- | The port to connect the corresponding lambda (by de Bruijn index) to
type FreeVar = (NodePort, Index)

-- | The interaction net
data InteractionNet
  = InteractionNet
    { heap :: Heap
    , connections :: AdjList
    }
  deriving (Eq, Show) --TODO: isomorphism of interaction nets


instance Semigroup InteractionNet where
  InteractionNet h1 l1 <> InteractionNet h2 l2 = InteractionNet (h1 <> h2) (l1 <> l2)

instance Monoid InteractionNet where
  mempty = InteractionNet M.empty []


-- | The global root node of any given graph will be here. This will
-- not actually be a node, but is the node that the main net connects
-- to.
rootNode :: NodePort
rootNode = ((-1), Principal)

-- | Generate a fresh address in the heap
fresh :: MonadState Addr m => m Addr
fresh = do
  a <- get
  modify (+1)
  return a

-- | Insert a node into the heap, returning its address
insertNode :: (MonadWriter InteractionNet m, MonadState Addr m) => Node -> m Addr
insertNode n = do
  addr <- fresh
  tell $ InteractionNet (M.singleton addr n) []
  return addr

insNode :: (MonadState Addr m) => Node -> InteractionNet -> m InteractionNet
insNode n (InteractionNet heap conns) = do
  addr <- fresh
  pure $ InteractionNet (M.insert addr n heap) conns

-- | Insert a new connection between two existing nodes into the graph
insertConn :: MonadWriter InteractionNet m => Connection -> m ()
insertConn conn = tell $ InteractionNet M.empty [conn]

insConn :: Connection -> InteractionNet -> InteractionNet
insConn conn (InteractionNet heap conns) = InteractionNet heap (conn:conns)

-- | Lookup a node in the heap
lookupNode :: (MonadReader InteractionNet m, MonadPlus m) => Addr -> m Node
lookupNode addr = do
  (InteractionNet heap _) <- ask
  toMplus $ M.lookup addr heap

-- | Lookup what port the given port is connected to
lookupConn :: (MonadReader InteractionNet m, MonadPlus m) => NodePort -> m Connection
lookupConn port = do
  (InteractionNet _ conns) <- ask
  (port,) <$> case lookup port conns of
    Nothing -> toMplus $ lookup port (swap <$> conns)
    Just p  -> pure p

--TODO: lenses
removeConn :: Connection -> InteractionNet -> InteractionNet
removeConn conn (InteractionNet heap conns)
  = InteractionNet heap (delete conn (delete (swap conn) conns))

--TODO: also delete connections to that node
removeNode :: Addr -> InteractionNet -> InteractionNet
removeNode addr (InteractionNet heap conns)
  = InteractionNet (M.delete addr heap) conns

highestAddr :: InteractionNet -> Addr
highestAddr (InteractionNet heap _) = maximum $ M.keys heap

-- | Get the node on the other end of a connection to the target
getConnectedTo :: (MonadReader InteractionNet m, MonadPlus m) => NodePort -> m NodePort
getConnectedTo port = do
  conn <- lookupConn port
  otherEnd port conn
  where
    otherEnd (taddr, tport) ((adr1, prt1), (adr2, prt2))
      | taddr == adr1 && tport == prt1 = pure (adr2, prt2)
      | taddr == adr2 && tport == prt2 = pure (adr2, prt2)
      | otherwise = mzero
