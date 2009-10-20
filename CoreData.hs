{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
--TODO: is this really neccessary?
{-# LANGUAGE UndecidableInstances #-}
module CoreData where

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
import qualified Control.Monad.State as S

data User = User {name :: String, password :: String, age :: Int} deriving (Show, Typeable)
data Post = Post {title :: String, body :: String, author :: BelongsTo User} deriving (Show, Typeable)

$(deriveAll ''User "PFUser")
type instance PF User = PFUser
$(mkLabels [''User])

$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost
$(mkLabels [''Post])

type Domain = (((), User), Post)
tUser = Suc Zero 
tPost = Zero 

db d = liftIO $ do conn <- connectSqlite3 "coredata.sqlite3"
                   x <- runDB conn d
                   commit conn
                   return x

-- Example

newtype DB' fam a = DB' {unDB' :: DB a}

instance MonadIO (DB' fam) where
  liftIO = DB' . liftIO

instance Monad (DB' fam) where
  return = DB' . return
  l >>= r = DB' (unDB' l >>= (fmap unDB' r))

instance Persist DB' () where
  pFetch _ _ = error "persist with broken index"

instance (Show a, Regular a,
         GModelName (PF a), 
         GColumns (PF a), 
         GParse (PF a),
         Persist DB' env) => Persist DB' (env, a) where
  pFetch x@Zero id = DB' $ find (tRefType x) id
  pFetch (Suc y) id = DB' $ unDB' $ pFetch y id


example :: CoreData DB' Domain ()
example = do user <- fetch tUser 1
             name <- user ? lName
             liftIO $ print "hi"

-- Types

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

type Type a    = a
type TypeName  = String
type UID       = Int
type Store fam = M.Map Int (M.Map UID Dynamic)

type CoreData p fam a = Persist p fam => S.StateT (Store fam) (p fam) a
newtype Ref fam a = Ref {unRef :: (TRef a fam, UID)}-- deriving Show

runCoreData :: Persist p fam => CoreData p fam a -> p fam ()
runCoreData comp = do (a, s) <- S.runStateT comp M.empty
                      return ()

fetch :: (Persist p fam, Regular a, GModelName (PF a)) => TRef a fam -> UID -> CoreData p fam (Ref fam a)
fetch typeWitness uid = return $ Ref (typeWitness, uid)

(?) :: (Persist p fam, Regular a, Typeable a) => Ref fam a -> (a :-> b) -> CoreData p fam b
ref ? prop = 
  do let (typeIndex, uid) = unRef ref
     x <- S.get
     case (M.lookup (tRefToInt typeIndex) x >>= M.lookup uid) of
          Nothing -> do result <- lift $ pFetch typeIndex uid 
                        case result of
                          Nothing -> error "Entity not found"
                          Just x  -> return $ get prop x -- TODO: update the map
          Just  x -> case fromDynamic x of
                          Nothing -> error "Internal error: (?)"
                          Just x  -> return $ get prop x
