> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , FunctionalDependencies
>   #-}

|
This module provides support for contextual processing (\"rendering\") of data
structures, built on top of the 'State' monad.

> module Data.Context (

* Classes

>   RenderC(..)
> , Context(..)

* Rendering Contexts
|
We provide some minimal rendering contexts from which more capable rendering
contexts may be created. For most purposes, it may be more appropriate to use
the contexts provided in the various submodules, which should correlate to
actual usage more closely.

> , AddC(..), liftAC, liftAC2

* Combinators

> , withPush

* Control.Monad.State
|
These functions allow for more direct manipulation of rendering contexts.

> , get
> , put

* Submodules
|
The following submodules may provide useful instance definitions and rendering
contexts:

  ["Data.Context.List"]
    Instances for 'RenderC' for processing lists.

  ["Data.Context.Text"]
    Contexts and combinators specific to processing strings.

> ) where

> import Control.Monad.State

> class Context s where
>   -- | The initial state of this rendering context.
>   zero  :: s
>   pushC :: s -> s -> s
>   popC  :: s -> s
>   push  :: s -> State s ()
>   pop   :: State s s
>   push c = do
>     old <- get
>     put $ pushC old c
>   pop = do
>     old <- get
>     put $ popC old
>     return old

> instance Context Bool where
>   zero = False
>   pushC a b = (a || b) && not (a && b)
>   popC = not

|
A simple rendering context that acts like a stack of empty units, allowing
one to gauge depth but nothing else.

> data Num n => AddC n = AddC n
>   deriving (Show, Eq)

> liftAC o (AddC a) = AddC $ o a
> liftAC2 o (AddC a) (AddC b) = AddC $ a `o` b

> instance Num n => Num (AddC n) where
>   (+) = liftAC2 (+)
>   (*) = liftAC2 (*)
>   abs = liftAC abs
>   signum = liftAC signum
>   fromInteger = AddC . fromInteger

> instance Num n => Context (AddC n) where
>   zero = 0
>   pushC = (+)
>   popC a = (-1)

> class Context c => RenderC d c o | d o -> c where
>   renderC     :: d -> State c o
>   renderFrom  :: d -> c -> o
>   render      :: d -> o
>   renderFrom d c = evalState (renderC d) c
>   render = flip renderFrom zero

| Executes a state in a context transformed by the given function.

> withPush :: (c -> c) -> State c o -> State c o
> withPush f s = do
>   old <- get
>   put $ f old
>   r <- s
>   put old
>   return r
