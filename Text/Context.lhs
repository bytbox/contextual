> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , FunctionalDependencies
>   , TemplateHaskell
>   #-}

> module Text.Context (

* Classes

>   RenderContext(..)
> , RenderC(..)

* Contexts
|
These are some very simple contexts from which more elaborate, useful contexts
may be constructed.

> , NumRC(..)

* Combinators

> , push

* Control.Monad.State
|
These functions permit direct manipulation of the rendering context, and need
not be used in general.

> , get
> , put
> ) where

> import Control.Monad.State
> import Data.Monoid

> import Util.TH

|
To be used in rendering, a context datatype must have an appropriate 'zero'
value.

> class RenderContext s where
>   initC :: s
>
> instance RenderContext () where
>   initC = ()
> 
> instance RenderContext [a] where
>   initC = []

|
A numeric rendering context.

> data Num n => NumRC n = NumRC n
>   deriving (Show, Eq)
>
> instance Num n => Num (NumRC n) where
>   (+) (NumRC a) (NumRC b) = NumRC $ a + b
>   (*) (NumRC a) (NumRC b) = NumRC $ a * b
>   abs (NumRC n) = NumRC $ abs n
>   signum (NumRC n) = NumRC $ signum n
>   fromInteger = NumRC . fromInteger
>
> instance Num n => RenderContext (NumRC n) where
>   initC = 0

> tupInstances [2..4] ''RenderContext 'initC

> class RenderContext s => RenderC d s o | d o -> s where
>   renderC       :: d -> State s o
>   renderFrom    :: d -> s -> o
>   render        :: d -> o
>   renderFrom d  = evalState (renderC d)
>   render        = flip renderFrom initC

> push :: (s -> s) -> State s o -> State s o
> push f st = do
>   c <- get
>   put $ f c
>   x <- st
>   put c
>   return x

