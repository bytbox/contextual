> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , UndecidableInstances
>   #-}

|
This module provides the 'RenderC' instance allowing lists of data to be rendered
into monoids. Note that we must use 'UndecidableInstances' to avoid the coverage
condition.

> module Data.Context.List (
>   RenderC
> ) where

> import Control.Monad
> import Data.Monoid

> import Data.Context

> instance (Monoid o, RenderC d c o) => RenderC [d] c o where
>   renderC [] = return mempty
>   renderC ds = liftM mconcat $ sequence $ map r ds
>     where
>       r = renderC

