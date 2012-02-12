{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Main where

import System.Exit (exitSuccess, exitFailure)
import Test.QuickCheck

import Data.Context

data AddState = AddState Int

instance Context AddState where
  zero = AddState 0
  pushC (AddState a) (AddState b) = AddState $ a + b
  popC (AddState a) = AddState $ a-1

instance RenderC Int AddState Int where
  renderC i = do
    AddState x <- get
    return $ i+x

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
