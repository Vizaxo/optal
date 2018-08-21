module Term where

-- | Lambda terms
data Term
  = TVar Index     -- ^ de Bruijn representation of variables (0-indexed)
  | TLam Term      -- ^ λbody
  | TApp Term Term -- ^ f x
  deriving Show

-- | The index used for de Bruijn indexing the terms
type Index = Int

