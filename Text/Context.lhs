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
> 
> instance RenderContext Int where
>   initC = 0
>
> tupInstances [2..4] ''RenderContext 'initC

> class RenderContext s => RenderC d s o | d -> s where
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
