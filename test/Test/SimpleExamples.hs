module Test.SimpleExamples where

import Compiler
import Decompiler
import Evaluate
import Examples

import Test.Tasty
import Test.Tasty.HUnit

simpleExamples :: TestTree
simpleExamples = testGroup "simple examples"
  [ testCase "identity is in normal form" idNormalForm]

assertNormalForm t = assertEqual "The term is not in normal form."
  (decompile (compile t))
  (decompile =<< eval (compile t))

idNormalForm = assertNormalForm identity
