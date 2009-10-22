{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Basil.Core where

import Generics.MultiRec.Base
import qualified Data.Map as M

data Ref     (phi :: * -> *) (ix :: *)
data RefList (phi :: * -> *) (ix :: *) where
  RLNil :: RefList phi ix

data One  = One
data Many = Many

data Relation multiplicity i1 i2 where
  Rel :: String                         -- | The name
      -> mult1                          -- | The multiplicity
      -> phi i1                         -- | The from
      -> phi i2                         -- | The to
      -> Relation (mult2, mult1) i2 i1  -- | The inverse
      -> Relation (mult1, mult2) i1 i2


data Nil (phi :: * -> *) a where Nil :: Nil phi a
data To mult1 mult2 (from :: *) (phi :: * -> *) to where
  To :: Relation (mult1, mult2) to from -> To mult1 mult2 from phi to

infixr 1 :&:
data (:&:) (a :: (* -> *) -> * -> *) 
           (b :: (* -> *) -> * -> *) 
           (phi :: * -> *) 
           (to :: *) :: * where
  (:&:) :: a phi to -> b phi to -> (:&:) a b phi to

type family Relations (fam :: * -> *) entity :: (* -> *) -> * -> *

class HasRelations phi ix | ix -> phi where -- TODO: relation should be on El.
  relations :: (Relations phi ix) phi ix

type family   Value (phi :: * -> *) (relation :: (* -> *) -> * -> *) :: *
type instance Value phi Nil = ()
type instance Value phi ((:&:) a b) = (Value phi a, Value phi b)
type instance Value phi ((Many `To` One)  ix) = RefList phi ix
type instance Value phi ((One  `To` One)  ix) = Ref phi ix
type instance Value phi ((One  `To` Many) ix) = Ref phi ix
type instance Value phi ((Many `To` Many) ix) = RefList phi ix

data Zero
data Suc a

data RelIndex (t :: (* -> *) -> * -> *) ix where
  RZero :: RelIndex t Zero
  RSuc  :: RelIndex t x -> RelIndex t (Suc x)

type family Index ix (relation :: (* -> *) -> * -> *) :: ((* -> *) -> * -> *)
type instance Index (RelIndex a Zero)  ((:&:) a b) = a
type instance Index (RelIndex t (Suc x)) ((:&:) a b) = Index (RelIndex t x) b

data Basil (phi :: * -> *) a  where
  Basil :: EnumTypes phi env => (State phi env, a) -> Basil phi a

--data State (phi :: * -> *) env

data Witnesses (phi :: * -> *) (env :: *) where
  WNil  :: Witnesses phi ()
  WCons :: El phi ix => Witnesses phi env -> Witnesses phi (ix, env)

newtype Id a = Id {unId :: a}

type family   State (phi :: * -> *) env :: *
type instance State phi () = ()
type instance State phi (x, xs) = (M.Map Int x, State phi xs)

emptyState :: Witnesses phi env -> State phi env
emptyState WNil       = ()
emptyState (WCons xs) = (M.empty, emptyState xs)


class EnumTypes phi env | phi -> env where 
  allTypes :: Witnesses phi env

runBasil :: forall phi env a . (EnumTypes phi env) => Basil phi a -> (State phi env, a)
runBasil x = (emptyState (allTypes :: Witnesses phi env), undefined)

find :: (El phi ix) => Int -> Basil phi (Maybe a)
find ix = undefined

new :: (El phi ix, HasRelations phi ix) => ix -> Value phi (Relations phi ix) -> Basil phi (Ref phi ix)
new = undefined

rel :: (El phi ix, HasRelations phi ix) => Ref phi ix -> relIndex -> Basil phi (Value phi (Index relIndex (Relations phi ix)))
rel = undefined
