{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Main where

import Data.Monoid
import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Text.Context

data Cont1 = Raw String | Sub Cont1 | Poly [Cont1]

myCont1 = Poly
  [ Raw "a"
  , Raw "b"
  , Sub $ Poly
    [ Raw "c"
    , Sub $ Raw "d"
    , Raw "e"
    ]
  , Raw "e"
  , Sub $ Raw "f"
  , Raw "g"
  ]

myOut1 = "a\nb\n  c\n    d\n  e\ne\n  f\ng\n"

indentRaw :: NumRC Int -> String -> String
indentRaw (NumRC n) c = (concat $ take n $ repeat "  ") ++ c ++ "\n"

instance RenderC Cont1 (NumRC Int) [Char] where
  renderC (Raw s) = get >>= return . flip indentRaw s
  renderC (Sub d) = push (\(NumRC n) -> NumRC $ n+1) $ renderC d
  renderC (Poly cs) = (sequence $ map renderC cs) >>= return . concat

testRender1 = TestCase $ assertEqual "Failure" myOut1 $ render myCont1

data PStream = PStream [PToken]
data PToken = PLeft | PRight | PRaw String
data ParenC = ParenC Bool

instance RenderContext ParenC where
  initC = ParenC False

instance RenderC PToken ParenC [Char] where
  renderC PLeft = do
    ParenC x <- get
    put $ ParenC $ not x
    return $ if x then "[" else "("
  renderC PRight = do
    ParenC x <- get
    put $ ParenC $ not x
    return $ if x then ")" else "]"
  renderC (PRaw s) = return s

instance RenderC [PToken] ParenC [[Char]] where
  renderC ds = sequence $ map renderC ds

instance RenderC [PToken] ParenC [Char] where
  renderC ds = renderC ds >>= return . concat

instance RenderC PStream ParenC [Char] where
  renderC (PStream s) = renderC s

myCont2 =
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

myOut2 = "a([][()]x)"

testRender2 = TestCase $ assertEqual "Failure" myOut2 $ render myCont2

tests = TestList
  [ TestLabel "Render1" testRender1
  , TestLabel "Render2" testRender2
  ]

main = do
  counts <- runTestTT tests
  if errors counts + failures counts > 0 then exitFailure else exitSuccess
