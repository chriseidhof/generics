{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- Unfortunately, this is needed for the nested type family application
{-# LANGUAGE UndecidableInstances #-}

module Basil.TContainerList where

-- Typed container list.

data TList (phi :: * -> *) a where
  TNil :: TList phi ()
  TCons :: phi x -> phi y -> f x y -> TList phi xs -> TList phi (f x y, xs)

data TBool a where
  TTrue  :: TBool True
  TFalse :: TBool False

data True
data False

-- There should be an instance for all combinations of the types in your domain.
type family TypeEq a b ::  *

type family   FilterIfTypeEq x xs :: *
type instance FilterIfTypeEq x () = ()
type instance FilterIfTypeEq x (f y z, ys) = AppendIfTrue (TypeEq x y `Or` TypeEq x z) (f y z) (FilterIfTypeEq x ys)

type family   AppendIfTrue bool x xs :: *
type instance AppendIfTrue True x xs  = (x, xs)
type instance AppendIfTrue False x xs = xs

type family Or x y :: *
type instance Or True  True  = True
type instance Or True  False = True
type instance Or False True  = True
type instance Or False False = False

class TEq phi where
  tEq :: phi x -> phi y -> TBool (TypeEq x y)

tOr :: TBool x -> TBool y -> TBool (Or x y)
tOr = undefined

filterByType :: TEq phi => phi x -> TList phi xs -> TList phi (FilterIfTypeEq x xs)
filterByType x TNil = TNil
filterByType x (TCons pr1 pr2 y ys) = case tOr (tEq x pr1) (tEq x pr2) of
                                 TTrue  -> TCons pr1 pr2 y (filterByType x ys)
                                 TFalse -> filterByType x ys -- x ys
