> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , FunctionalDependencies
>   #-}

> module Data.Context (

* Classes

>   RenderC(..)
> , Context(..)

* Rendering Contexts

> , AddC(..)

* Combinators

> , withPush

* Control.Monad.State

> , get
> , put

* Submodules

> , module Data.Context.Text

> ) where

> import Control.Monad.State

> import Data.Context.Text

> class Context s where
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

> withPush :: (c -> c) -> State c o -> State c o
> withPush f s = do
>   old <- get
>   put $ f old
>   r <- s
>   put old
>   return r
