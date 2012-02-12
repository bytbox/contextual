> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , FlexibleContexts
>   , UndecidableInstances
>   #-}

|
This module provides the 'RenderC' instance allowing existing 'RenderC'
instances that output to 'String's to output to 'ByteString's. Note that we
must use 'UndecidableInstances' to avoid the coverage condition.

> module Data.Context.ByteString (
>   RenderC
> ) where

> import Data.List (foldl')
> import Data.String (fromString)
> import qualified Data.ByteString.Char8 as B

> import Data.Context

|
Note that this instance does not provide any actual speed advantage - the
conversion to 'ByteString' is done after the call to the parent 'renderC'.

> instance RenderC d c String => RenderC d c B.ByteString where
>   renderC d = return . fromString =<< renderC d

> instance RenderC d c String => RenderC [d] c B.ByteString where
>   renderC d = return . foldl' B.append B.empty =<< (sequence $ map renderC d)

