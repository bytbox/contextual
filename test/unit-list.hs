{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Data.Context
import Data.Context.List

data PStream = PStream [PToken]
data PToken = PLeft | PRight | PRaw String
type ParenC = Bool

instance RenderC PToken ParenC [Char] where
  renderC PLeft = do
    x <- get
    put $ not x
    return $ if x then "[" else "("
  renderC PRight = do
    x <- get
    put $ not x
    return $ if x then ")" else "]"
  renderC (PRaw s) = return s

myCont1 =
  [ PRaw "a"
  , PLeft
  , PLeft
  , PRight
  , PLeft
  , PLeft
  , PRight
  , PRight
  , PRaw "x"
  , PRight
  ]

myOut1 = "a([][()]x)"

testRender1 = TestCase $ assertEqual "Failure" myOut1 $ render myCont1

tests = TestList
  [ testRender1
  ]

main = do
  counts <- runTestTT tests
  if errors counts + failures counts > 0 then exitFailure else exitSuccess
