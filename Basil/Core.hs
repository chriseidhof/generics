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

import Basil.TContainerList
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

class Fam phi => ERModel (phi :: * -> *) env | phi -> env, env -> phi where
  relations :: TList phi env

relationsForType :: (ERModel phi xs, TEq phi) => phi x -> TList phi (FilterIfTypeEq x xs)
relationsForType ix = filterByType ix relations

data Relation multiplicity i1 i2 where
  Rel :: String                         -- | The name
      -> mult1                          -- | The multiplicity
      -> phi i2                         -- | The to
      -> Relation (mult2, mult1) i2 i1  -- | The inverse
      -> Relation (mult1, mult2) i1 i2

data Nil (phi :: * -> *) a where Nil :: Nil phi a

data To  mult1 mult2 l r where
  Relation :: Relation (mult1, mult2) l r 
           -> Relation (mult2, mult1) r l
           -> To  mult1 mult2 l r
mkRelation x y = let x1 = x y1
                     y1 = y x1
                 in Relation x1 y1

-- type family   Value (phi :: * -> *) (relation :: (* -> *) -> * -> *) :: *
-- type instance Value phi Nil = ()
-- type instance Value phi ((:&:) a b) = (Value phi a, Value phi b)
-- type instance Value phi ((Many `To` One)  ix) = RefList phi ix
-- type instance Value phi ((One  `To` One)  ix) = Ref phi ix
-- type instance Value phi ((One  `To` Many) ix) = Ref phi ix
-- type instance Value phi ((Many `To` Many) ix) = RefList phi ix

data Zero
data Suc a

data Witnesses (phi :: * -> *) (env :: *) where
  WNil  :: Witnesses phi ()
  WCons :: El phi ix => Witnesses phi env -> Witnesses phi (ix, env)


class (Monad (p phi), Fam phi) => Persist (p :: (* -> *) -> * -> *) (phi :: * -> *) where
  pFetch :: phi ix -> Int -> p phi (Maybe ix)
  -- pSave  :: Regular a => TRef f a fam -> Int -> a -> p fam ()
  -- pFetchHasMany :: (Regular a, Regular b) => TRef TypeCache b fam -> NamedLabel a (Many b) -> Int -> p fam [(Int, b)]
