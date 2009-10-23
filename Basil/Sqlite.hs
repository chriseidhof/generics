{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Basil.Sqlite where

import Control.Applicative
import Control.Monad.Trans (liftIO, lift, MonadIO)
import CoreData.Core
import CoreData.TRef
import Database.HDBC (commit)
import Database.HDBC (toSql, fromSql)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Generics.Regular
import Generics.Regular.Database
import Generics.Regular.ModelName
import qualified Generics.Regular.Database.Columns as C

newtype DB' fam a = DB' {unDB' :: DB a}

instance MonadIO (DB' fam) where
  liftIO = DB' . liftIO

instance Monad (DB' fam) where
  return = DB' . return
  l >>= r = DB' (unDB' l >>= (fmap unDB' r))

instance Persist DB' () where
  pFetch        = error "persist with broken index"
  pFetchHasMany = error "persist with broken index"
  pSave         = error "persist with broken index"

instance (Show a, Regular a,
         GModelName (PF a), 
         GColumns (PF a), 
         GParse (PF a),
         GValues (PF a),
         Persist DB' env) => Persist DB' (f a, env) where

  pFetch x@Zero id = DB' $ find (tRefType x) id
  pFetch (Suc y) id = DB' $ unDB' $ pFetch y id
  pFetchHasMany x@(Zero) label ix = DB' $ fillHasMany ix (key label)
  pFetchHasMany  (Suc x) label ix = DB' $ unDB' $ pFetchHasMany x label ix

  pSave x@(Zero) ix val = DB' $ update val ix
  pSave  (Suc x) ix val = DB' $ unDB' $ pSave x ix val

instance ParseSql  (One a) where parsef = Just <$> ((One . fromSql) <$> getOne) -- Can this be done easier?
instance C.Columns (One a) where columns _ l  = [l ++ "_id"]
instance Values    (One a) where values = return . toSql . unOne

instance ParseSql  (Many a) where parsef = return (Just HM)
instance C.Columns (Many a) where columns _ l  = []
instance Values    (Many a) where values =  const []

fillHasMany :: ( Regular c, GParse (PF c), GColumns (PF c)
                 , GModelName (PF c), Show c
                 ) => Int
                   -> String
                   -> DB [(Int, c)]
fillHasMany ix bt = findAll undefined [(bt, toSql ix)]

