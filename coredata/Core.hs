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

import Data.Record.Label
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
import qualified Control.Monad.State as S

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
tRefType = undefined

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
type Store fam = M.Map Int (M.Map UID Dynamic)
type Tainted fam = M.Map Int (S.Set UID)
data State fam = State {store :: Store fam, tainted :: Tainted fam}
$(mkLabels [''State])

type CoreData p fam a = Persist p fam => S.StateT (State fam) (p fam) a
newtype Ref fam a = Ref {unRef :: (TRef a fam, UID)}-- deriving Show

instance Show (Ref fam a) where
  show (Ref (t, id)) = "Ref {" ++ show (tRefToInt t) ++ ", " ++ show id ++ "}"

runCoreData :: Persist p fam => CoreData p fam a -> p fam ()
runCoreData comp = do (a, s) <- S.runStateT comp (State M.empty M.empty)
                      return ()

fetch :: (Persist p fam, Regular a, GModelName (PF a)) => TRef a fam -> UID -> CoreData p fam (Ref fam a)
fetch typeWitness uid = return $ Ref (typeWitness, uid)


(?) :: (Persist p fam, Regular a, Typeable a) => Ref fam a -> (a :-> b) -> CoreData p fam b
ref ? prop = 
  do let (typeIndex, uid) = unRef ref
     x <- getM lStore
     case (M.lookup (tRefToInt typeIndex) x >>= M.lookup uid) of
          Nothing -> do result <- lift $ pFetch typeIndex uid 
                        case result of
                          Nothing -> error "Entity not found"
                          Just x  -> do cache ref x
                                        return $ get prop x -- TODO: update the map
          Just  x -> case fromDynamic x of
                          Nothing -> error "Internal error: (?)"
                          Just x  -> return $ get prop x

set :: (Persist p fam, Regular a, Typeable a) => Ref fam a -> (a :-> b) -> b -> CoreData p fam ()
set ref setter newValue =  do
  let (typeIndex, uid) = unRef ref
  tainted <- getM lTainted
  undefined -- TODO


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
  ref <@> relation = do
    let (_, uid) = unRef ref
    many <- lift $ pFetchHasMany uid index relation
    return $ fromList $ map (Ref . ((,) index)) $ map fst many


-- RefLists (implementation may (will) change)

newtype RefList fam a = RefList {unRefList :: [(Ref fam a)]}

fromList :: [(Ref fam a)] -> RefList fam a
fromList = RefList

count :: RefList fam a -> Int
count = length . unRefList

objectAtIndex :: RefList fam a -> Int -> Maybe (Ref fam a)
objectAtIndex l ix = Just (unRefList l !! ix)
