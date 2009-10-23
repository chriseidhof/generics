{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Basil.Cache where

import Basil.Core
import Data.Record.Label (mkLabels, label)
import qualified Data.Map as M
import qualified Data.Set as S

data TypeCache a = TypeCache {_cached :: M.Map Ident a, _tainted :: S.Set Ident} deriving Show

type family   Cache (phi :: * -> *) env :: *
type instance Cache phi () = ()
type instance Cache phi (x, xs) = (TypeCache x, Cache phi xs)

emptyState :: Witnesses phi env -> Cache phi env
emptyState WNil       = ()
emptyState (WCons xs) = (TypeCache M.empty S.empty, emptyState xs)

data CacheIndex (phi :: * -> *) ix env where
  Zero :: CacheIndex phi ix (ix, env)
  Suc  :: CacheIndex phi ix env' -> CacheIndex phi ix (b, env')

lookupCache :: CacheIndex phi ix env
                 -> Cache phi env
                 -> TypeCache ix
lookupCache Zero = fst
lookupCache (Suc x) = lookupCache x . snd

modCache :: (TypeCache ix -> TypeCache ix)
            -> CacheIndex phi ix env
            -> Cache phi env
            -> Cache phi env
modCache f Zero (a,b) = (f a, b)
modCache f (Suc x) (a,b) = (a, modCache f x b)

class EnumTypes phi env | phi -> env where 
  allTypes :: Witnesses phi env
  index :: phi ix -> CacheIndex phi ix env

$(mkLabels [''TypeCache])
