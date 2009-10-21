{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CoreData.TRef (TRef (..), lookupTRef, modTRef, tRefType, Index (..)) where

import Control.Applicative 
import Generics.Regular

class Index f typ fam where
  index :: TRef f typ fam

data TRef (f :: * -> *) (a :: *) (env :: *) where
 Zero :: TRef f a (f a, env')
 Suc  :: TRef f a env' -> TRef f a (f b, env')

lookupTRef :: TRef f a env -> env -> f a
lookupTRef Zero = fst
lookupTRef (Suc x) = lookupTRef x . snd

modTRef :: TRef f a env -> (f a -> f a) -> env -> env
modTRef Zero    f (a,b) = (f a, b)
modTRef (Suc x) f (a,b)= (a, modTRef x f b)

instance Show (TRef f a env) where
  show x = "TRef {" ++ show (tRefToInt x) ++ "}"

tRefToInt :: TRef f a env -> Int
tRefToInt Zero = 0
tRefToInt (Suc x) = 1 + (tRefToInt x)

tRefType :: TRef f a env -> a
tRefType = error "trying to evaluate a tRefType"

