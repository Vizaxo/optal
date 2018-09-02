module Main where

import Test.Compiler
import Test.SimpleExamples

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [simpleExamples
  ,compiler
  ]
