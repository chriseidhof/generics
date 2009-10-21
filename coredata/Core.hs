{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module CoreData.Core where

import Control.Applicative
import Data.Record.Label hiding (set)
import qualified Data.Record.Label as L
import Generics.Regular
import Generics.Regular.Database
import Generics.Regular.Relations
import Generics.Regular.ModelName
import Control.Monad.Trans (liftIO, lift, MonadIO)
import Database.HDBC (commit)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as ST
import Prelude hiding ((.), mod)
import Control.Category
import CoreData.TRef

-- TODO
--   * Setters for relations <@=>
--   * Save state to database
--   * Undo/redo
--   * Fetching with predicate

-- Types

data Zero
data Suc a


tRefToInt :: TRef f a env -> Int
tRefToInt Zero = 0
tRefToInt (Suc x) = 1 + (tRefToInt x)

tRefType :: TRef f a env -> a
tRefType = error "trying to evaluate a tRefType"

class (Monad (p fam)) => Persist p fam where
  pFetch :: Regular a => TRef f a fam -> Int -> p fam (Maybe a)
  pFetchHasMany :: (Regular a, Regular b) => Int -> TRef TypeCache b fam -> NamedLabel a (HasMany b) -> p fam [(Int, b)]

class Index f typ fam where
  index :: TRef f typ fam
 
data NamedLabel a b = NamedLabel {rel :: a :-> b, key :: String}
type Type a    = a
type TypeName  = String
type UID       = Int
data Ident = UID UID | Fresh Int deriving (Ord, Show, Eq)
data State fam = State {tCache :: fam, freshId :: Int} deriving Show
data TypeCache a = TypeCache {store :: M.Map Ident a, tainted :: S.Set Ident} deriving Show
 
type CoreData p fam a = Persist p fam => ST.StateT (State fam) (p fam) a
newtype Ref f fam a = Ref {unRef :: (TRef f a fam, Ident)}-- deriving Show

emptyState :: TRef TypeCache a env -> env
emptyState Zero    = (error "Empty state: wrong index.", TypeCache M.empty S.empty)
emptyState (Suc x) = (emptyState x, TypeCache M.empty S.empty)
 
$(mkLabels [''State])
$(mkLabels [''TypeCache])
 
instance Show (Ref f fam a) where
  show (Ref (t, id)) = "Ref {" ++ show (tRefToInt t) ++ ", " ++ show id ++ "}"

runCoreData :: (Persist p fam) => CoreData p fam a -> TRef TypeCache b fam -> p fam ()
runCoreData comp max = do (a, s) <- ST.runStateT comp (State (emptyState max) 0)
                          return ()
-- 
-- saveCoreData :: (MonadIO (p fam), Show a, Show (State fam)
--                 ,Persist p fam) => CoreData p fam a -> p fam ()
-- saveCoreData comp = do (a, s) <- ST.runStateT comp (State M.empty M.empty 0)
--                        todo "saveCoreData"
--                        return ()
-- 
fetch :: (Persist p fam, Regular a, GModelName (PF a)) => TRef TypeCache a fam -> UID -> CoreData p fam (Ref TypeCache fam a)
fetch typeWitness uid = return $ Ref (typeWitness, UID uid)
 
create :: (Persist p fam, Regular a, GModelName (PF a)) => TRef TypeCache a fam -> a -> CoreData p fam (Ref TypeCache fam a)
create tRef val = do ref <- mkFreshId tRef
                     cache ref val
                     addTainted ref
                     return ref

(<@>) :: (Persist p fam, Regular a) => Ref TypeCache fam a -> (a :-> b) -> CoreData p fam b
ref <@> prop = 
  do let (typeIndex, ident) = unRef ref
     x <- getM lTCache
     case (M.lookup ident $ store $ lookupTRef typeIndex x) of
          Nothing -> get prop <$> force ref
          Just  x -> return $ get prop x

set :: (Persist p fam, Regular a) => Ref TypeCache fam a -> (a :-> b) -> b -> CoreData p fam ()
set ref setter newValue =  do
  let (typeIndex, uid) = unRef ref
      tIndex = tRefToInt typeIndex
  addTainted ref
  val <- force ref
  withCache ref $ \uid -> mod lStore (M.insert uid $ L.set setter newValue val)
  
-- Internal methods.

mkFreshId :: TRef TypeCache a fam -> CoreData p fam (Ref TypeCache fam a)
mkFreshId tRef = do x <- getM lFreshId
                    modM lFreshId (+1)
                    return $ Ref (tRef, Fresh x)

-- TODO: make sure force isn't exposed.
force :: (Persist p fam, Regular a) => Ref TypeCache fam a -> CoreData p fam a
force ref@(Ref (typeIndex, ident)) = do m <- getM lTCache
                                        case M.lookup ident $ store $ lookupTRef typeIndex m of
                                             Just x  -> return x
                                             Nothing -> case ident of
                                                             UID uid -> do result <- lift $ pFetch typeIndex uid 
                                                                           case result of
                                                                                Nothing -> error "Entity not found"
                                                                                Just x  -> do cache ref x
                                                                                              return x
                                                             Fresh id -> error "Internal error: trying to force a fresh item that doesn't exist."
  

addTainted :: Ref TypeCache fam a -> CoreData p fam ()
addTainted ref@(Ref (i1,i2)) = withCache ref $ mod lTainted . S.insert

-- TODO: this can probably be optimized some more.
cache :: Ref TypeCache fam a -> a -> CoreData p fam ()
cache ref val = withCache ref (\uid -> (mod lStore $ M.insert uid (val)))

withCache (Ref (typeIndex, uid)) f = modM lTCache $ modTRef typeIndex (f uid)

-- TODO: separate module. The relations are good, but this shouldn't be tied to a database.

class Relation arr wrap res where
  (?) :: (Persist p fam, Regular a, GModelName (PF a), Regular b, GModelName (PF b), Index TypeCache b fam) 
        => Ref TypeCache fam a -> arr a (wrap b) -> CoreData p fam (res TypeCache fam b)

instance Relation (:->) BelongsTo Ref where
 ref ? relation = do 
   (BTId x) <- ref <@> relation
   fetch index x

instance Relation NamedLabel HasMany RefList where
  ref ? relation = 
    let (_, ident) = unRef ref in
    case ident of
      UID uid -> do many <- lift $ pFetchHasMany uid index relation
                    -- TODO : store all of them.
                    return $ fromList $ map (Ref . ((,) index) . UID) $ map fst many
      Fresh x -> todo "HasMany not implemented yet"

-- Temporary stuff
todo x = error $ "TODO: " ++ x

-- RefLists (implementation may (will) change)

newtype RefList f fam a = RefList {unRefList :: [(Ref f fam a)]} deriving Show

fromList :: [(Ref f fam a)] -> RefList f fam a
fromList = RefList

count :: RefList f fam a -> Int
count = length . unRefList

objectAtIndex :: RefList f fam a -> Int -> Maybe (Ref f fam a)
objectAtIndex l ix = Just (unRefList l !! ix)
