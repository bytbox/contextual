{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Data.Context

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

myOut1 = "a\nb\n c\n  d\n e\ne\n f\ng\n"

indentRaw :: AddC Int -> String -> String
indentRaw (AddC n) c = (take n $ repeat ' ') ++ c ++ "\n"

instance RenderC Cont1 (AddC Int) [Char] where
  renderC (Raw s) = get >>= return . flip indentRaw s
  renderC (Sub d) = withPush (+1) $ renderC d
  renderC (Poly cs) = (sequence $ map renderC cs) >>= return . concat

testRender1 = TestCase $ assertEqual "Failure" myOut1 $ render myCont1

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
