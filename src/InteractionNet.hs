module InteractionNet where

import Term

import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M


-- | Nodes in the graph
data NodeType
  = NodLam -- ^ Principal: root, Secondary: body, Tertiary: var
  | NodApp -- ^ Principal: f. Secondary: x. Tertiary: root
  | NodFan -- ^ Principal: single-end. Secondary: shared side a. Tertiary: shared side b
  deriving Show

-- | Each node has a type and an associated index
data Node
  = Node
    { nodeType :: NodeType
    , index :: Int
    }
  deriving Show

-- | Ports of graph nodes
data Port = Principal | Secondary | Tertiary
  deriving Show

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
data InteractionNet = InteractionNet Heap AdjList
  deriving Show


instance Semigroup InteractionNet where
  InteractionNet h1 l1 <> InteractionNet h2 l2 = InteractionNet (h1 <> h2) (l1 <> l2)

instance Monoid InteractionNet where
  mempty = InteractionNet M.empty []


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

-- | Insert a new connection between two existing nodes into the graph
insertConn :: MonadWriter InteractionNet m => Connection -> m ()
insertConn conn = tell $ InteractionNet M.empty [conn]
