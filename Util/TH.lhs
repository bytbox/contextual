> module Util.TH where

> import Control.Monad (replicateM)
> import Language.Haskell.TH

> tupInstance :: Int -> Name -> Name -> DecQ
> tupInstance i cls fn = do
>   tnl <- replicateM i (newName "t")
>   let tl = map varT tnl
>   let t = foldl appT (tupleT i) tl
>   instanceD (cxt $ map (\x -> classP cls [x]) tl) (appT (conT cls) t)
>     [funD fn [clause [] (normalB $ tupE $ replicate i $ global fn) []]]

> tupInstances :: [Int] -> Name -> Name -> Q [Dec]
> tupInstances is cls fn = sequence $ map (\i -> tupInstance i cls fn) is
