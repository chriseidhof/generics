{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module CoreData.Core where

import Control.Applicative
import Data.Record.Label hiding (set)
import qualified Data.Record.Label as L
import Generics.Regular
import Generics.Regular.Database
import Generics.Regular.Relations
import Generics.Regular.ModelName
import Data.Dynamic
import Control.Monad.Trans (liftIO, lift, MonadIO)
import Database.HDBC (commit)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as ST

-- Types

data Zero
data Suc a

data Nil
data TRef a env where
 Zero :: TRef a (env', a)
 Suc  :: TRef a env' -> TRef a (env', b)

tRefToInt :: TRef a env -> Int
tRefToInt Zero = 0
tRefToInt (Suc x) = 1 + (tRefToInt x)

tRefType :: TRef a env -> a
tRefType = error "trying to evaluate a tRefType"

class (Monad (p fam)) => Persist (p :: * -> * -> *) fam where
  pFetch :: Regular a => TRef a fam -> Int -> p fam (Maybe a)
  -- todo: the next line is ugly.
  pFetchHasMany :: (Regular a, Regular b) => Int -> TRef b fam -> NamedLabel a (HasMany b) -> p fam [(Int, b)]

class Index typ fam where
  index :: TRef typ fam

data NamedLabel a b = NamedLabel {rel :: a :-> b, key :: String}
type Type a    = a
type TypeName  = String
type UID       = Int
data Ident = UID UID | Fresh Int deriving (Ord, Show, Eq)
type Store fam = M.Map Int (M.Map Ident Dynamic)
type Tainted fam = M.Map Int (S.Set Ident)
data State fam = State {store :: Store fam, tainted :: Tainted fam, freshId :: Int} deriving Show
$(mkLabels [''State])

type CoreData p fam a = Persist p fam => ST.StateT (State fam) (p fam) a
newtype Ref fam a = Ref {unRef :: (TRef a fam, Ident)}-- deriving Show

instance Show (Ref fam a) where
  show (Ref (t, id)) = "Ref {" ++ show (tRefToInt t) ++ ", " ++ show id ++ "}"

runCoreData :: (MonadIO (p fam), Show a, Show (State fam) -- debugging
               ,Persist p fam) => CoreData p fam a -> p fam ()
runCoreData comp = do (a, s) <- ST.runStateT comp (State M.empty M.empty 0)
                      liftIO $ print (a, s)
                      return ()

saveCoreData :: (MonadIO (p fam), Show a, Show (State fam)
                ,Persist p fam) => CoreData p fam a -> p fam ()
saveCoreData comp = do (a, s) <- ST.runStateT comp (State M.empty M.empty 0)
                       todo "saveCoreData"
                       return ()

fetch :: (Persist p fam, Regular a, GModelName (PF a)) => TRef a fam -> UID -> CoreData p fam (Ref fam a)
fetch typeWitness uid = return $ Ref (typeWitness, UID uid)

create :: (Persist p fam, Regular a, Typeable a, GModelName (PF a)) => TRef a fam -> a -> CoreData p fam (Ref fam a)
create tRef val = do ref <- mkFreshId tRef
                     cache ref val
                     addTainted ref
                     return ref

(?) :: (Persist p fam, Regular a, Typeable a) => Ref fam a -> (a :-> b) -> CoreData p fam b
ref ? prop = 
  do let (typeIndex, uid) = unRef ref
     x <- getM lStore
     case (M.lookup (tRefToInt typeIndex) x >>= M.lookup uid) of
          Nothing -> get prop <$> force ref
          Just  x -> case fromDynamic x of
                          Nothing -> error "Internal error: (?)"
                          Just x  -> return $ get prop x

set :: (Persist p fam, Regular a, Typeable a) => Ref fam a -> (a :-> b) -> b -> CoreData p fam ()
set ref setter newValue =  do
  let (typeIndex, uid) = unRef ref
      tIndex = tRefToInt typeIndex
  addTainted ref
  val <- force ref
  modM lStore   $ insert' (tIndex, uid, L.set setter newValue val)
  
-- Internal methods.

mkFreshId :: TRef a fam -> CoreData p fam (Ref fam a)
mkFreshId tRef = do x <- getM lFreshId
                    modM lFreshId (+1)
                    return $ Ref (tRef, Fresh x)

-- TODO: force can check whether the item's already in the cache.
-- TODO: make sure force isn't exposed.
force :: (Persist p fam, Regular a, Typeable a) => Ref fam a -> CoreData p fam a
force ref@(Ref (typeIndex, Fresh x)) = do m <- getM lStore
                                          case (M.lookup (tRefToInt typeIndex) m >>= M.lookup (Fresh x) >>= fromDynamic) of
                                               Nothing -> error "Internal error: trying to force a fresh item that doesn't exist."
                                               Just x  -> return x

force ref@(Ref (typeIndex, UID uid)) = do 
  result <- lift $ pFetch typeIndex uid 
  case result of
       Nothing -> error "Entity not found"
       Just x  -> do cache ref x
                     return x
        
  

insert' (i1,i2,val) m = case M.lookup i1 m of
                          Nothing -> M.insert i1 (M.singleton i2  (toDyn val)) m
                          Just x  -> M.update (Just . M.insert i2 (toDyn val)) i1 m

addTainted (Ref (i1,i2)) = modM lTainted $ \m -> case M.lookup (tRefToInt i1) m of
  Nothing -> M.insert (tRefToInt i1) (S.singleton i2 ) m
  Just x  -> M.update (Just . S.insert i2) (tRefToInt i1) m

-- TODO: this can probably be optimized some more.
cache :: (Typeable a) => Ref fam a -> a -> CoreData p fam ()
cache ref val = do
  store' <- getM lStore
  let (typeIndex, uid) = unRef ref
      index = tRefToInt typeIndex
  case M.lookup index store' of
       Nothing -> setM lStore (M.insert index (M.singleton uid $ toDyn val) store')
       Just x  -> setM lStore (M.update (Just . M.insert uid (toDyn val)) index store')

-- TODO: separate module. The relations are good, but this shouldn't be tied to a database.

class Relation arr wrap res where
  (<@>) :: (Persist p fam, Regular a, Typeable a, GModelName (PF a), Regular b, GModelName (PF b), Index b fam) 
        => Ref fam a -> arr a (wrap b) -> CoreData p fam (res fam b)

instance Relation (:->) BelongsTo Ref where
  ref <@> relation = do 
    (BTId x) <- ref ? relation
    fetch index x

instance Relation NamedLabel HasMany RefList where
  ref <@> relation = 
    let (_, ident) = unRef ref in
    case ident of
      UID uid -> do many <- lift $ pFetchHasMany uid index relation
                    return $ fromList $ map (Ref . ((,) index) . UID) $ map fst many
      Fresh x -> todo "HasMany not implemented yet"

-- Temporary stuff
todo x = error $ "TODO: " ++ x

-- RefLists (implementation may (will) change)

newtype RefList fam a = RefList {unRefList :: [(Ref fam a)]}

fromList :: [(Ref fam a)] -> RefList fam a
fromList = RefList

count :: RefList fam a -> Int
count = length . unRefList

objectAtIndex :: RefList fam a -> Int -> Maybe (Ref fam a)
objectAtIndex l ix = Just (unRefList l !! ix)
