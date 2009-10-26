{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
-- Unfortunately, this is needed for the nested type family application
{-# LANGUAGE UndecidableInstances #-}

module Basil.TContainerList where

-- Typed container list.

data TList (phi :: * -> *) a where
  TNil :: TList phi ()
  TCons :: phi x -> f x -> TList phi xs -> TList phi (f x, xs)

data TBool a where
  TTrue  :: TBool True
  TFalse :: TBool False

data True
data False

-- There should be an instance for all combinations of the types in your domain.
type family TypeEq a b ::  *

type family   FilterIfTypeEq x xs :: *
type instance FilterIfTypeEq x () = ()
type instance FilterIfTypeEq x (f y, ys) = AppendIfTrue (TypeEq x y) (f y) (FilterIfTypeEq x ys)

type family   AppendIfTrue bool x xs :: *
type instance AppendIfTrue True x xs  = (x, xs)
type instance AppendIfTrue False x xs = xs

class TEq phi where
  tEq :: phi x -> phi y -> TBool (TypeEq x y)


filterByType :: TEq phi => phi x -> TList phi xs -> TList phi (FilterIfTypeEq x xs)
filterByType x TNil = TNil
filterByType x (TCons pr y ys) = case tEq x pr of
                                 TTrue  -> TCons pr y (filterByType x ys)
                                 TFalse -> filterByType x ys -- x ys

