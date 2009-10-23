{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE FlexibleContexts   #-}

module Basil.Core where

import Generics.MultiRec.Base hiding (index)
import Control.Monad.Trans (lift)
import Data.Record.Label hiding (set)
import qualified Data.Map as M
import qualified Control.Monad.State as ST

data Ref     (phi :: * -> *) (ix :: *) where
  Ref :: (phi ix) -> Ident -> Ref phi ix
data RefList (phi :: * -> *) (ix :: *) where
  RLNil :: RefList phi ix

data Ident = UID UID | Fresh Int deriving (Ord, Show, Eq)
type UID   = Int

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

data Witnesses (phi :: * -> *) (env :: *) where
  WNil  :: Witnesses phi ()
  WCons :: El phi ix => Witnesses phi env -> Witnesses phi (ix, env)

class (Monad (p phi), Fam phi) => Persist (p :: (* -> *) -> * -> *) (phi :: * -> *) where
  pFetch :: phi ix -> Int -> p phi (Maybe ix)
  -- pSave  :: Regular a => TRef f a fam -> Int -> a -> p fam ()
  -- pFetchHasMany :: (Regular a, Regular b) => TRef TypeCache b fam -> NamedLabel a (Many b) -> Int -> p fam [(Int, b)]
