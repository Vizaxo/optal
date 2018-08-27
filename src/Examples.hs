module Examples where

import Term

-- | λ0
identity :: Term
identity = TLam (TVar 0)

-- | λ0 0
appSelf :: Term
appSelf = TLam (TApp (TVar 0) (TVar 0))

-- | λλ1
fst' :: Term
fst' = TLam (TLam (TVar 1))

-- | λλ0
snd' :: Term
snd' = TLam (TLam (TVar 0))

-- | (λ0)(λ0)
idAppId :: Term
idAppId = TApp identity identity
