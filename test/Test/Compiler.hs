module Test.Compiler where

import Compiler
import Decompiler

import Test.Generators

import Test.Tasty
import Test.Tasty.QuickCheck

compiler :: TestTree
compiler = testGroup "compiler"
  [testProperty "compiling then decompiling doesn't change the original term" compileDecompile]

compileDecompile term = case decompile (compile term) of
  Nothing -> False ==> True
  Just term' -> term === term'
