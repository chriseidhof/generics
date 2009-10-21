{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module CoreData.TRef (TRef (..), lookupTRef, modTRef, tRefType) where

data TRef (f :: * -> *) (a :: *) (env :: *) where
 Zero :: TRef f a (env', f a)
 Suc  :: TRef f a env' -> TRef f a (env', f b)

lookupTRef :: TRef f a env -> env -> f a
lookupTRef Zero = snd
lookupTRef (Suc x) = lookupTRef x . fst

modTRef :: TRef f a env -> (f a -> f a) -> env -> env
modTRef Zero    f (a,b) = (a, f b)
modTRef (Suc x) f (a,b)= (modTRef x f a, b)

instance Show (TRef f a env) where
  show x = "TRef {" ++ show (tRefToInt x) ++ "}"

tRefToInt :: TRef f a env -> Int
tRefToInt Zero = 0
tRefToInt (Suc x) = 1 + (tRefToInt x)

tRefType :: TRef f a env -> a
tRefType = error "trying to evaluate a tRefType"
