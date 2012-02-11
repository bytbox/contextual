{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import System.Exit (exitSuccess, exitFailure)
import Test.QuickCheck

import Text.Context

data AddState = AddState Int

instance RenderContext AddState where
  initC = AddState 0

instance RenderC Int AddState Int where
  renderC i = do
    AddState a <- get
    return $ a+i

rAdd :: Int -> Int -> Int
rAdd a b = renderFrom a $ AddState b

posSmallInt :: Gen Int
posSmallInt = choose (1, 5)

props :: [Property]
props =
  [ property (\a b -> (rAdd a b) == a+b)
  ]

isSuccess :: Result -> Bool
isSuccess (Success _ _ _) = True
isSuccess _ = False

main = do
  rs <- sequence $ map (\p -> quickCheckResult p >>= return . isSuccess) props
  if and rs then exitSuccess else exitFailure
--main = foldl (>>) (return ()) checks
