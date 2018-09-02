module Test.Generators where

import Term

import Test.QuickCheck

-- TODO: use size to control the term size

genTerm :: Gen Term
genTerm = frequency [(3, genVar), (3, genLam), (2, genApp)]

genVar :: Gen Term
genVar = (TVar . abs) <$> arbitrary

genLam :: Gen Term
genLam = TLam <$> genTerm

genApp :: Gen Term
genApp = TApp <$> genTerm <*> genTerm

instance Arbitrary Term where
  arbitrary = genTerm
