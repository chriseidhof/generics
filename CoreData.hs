--TODO: is this really neccessary?
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CoreData where

import Data.Record.Label (mkLabels, label)
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
import CoreData.Core
import CoreData.TRef

-- Types


-- Example
--
type Domain f = (((), f User), f Post)
data User = User {name :: String, password :: String, age :: Int, posts :: HasMany Post} deriving (Show, Typeable)
data Post = Post {title :: String, body :: String, author :: BelongsTo User} deriving (Show, Typeable)

$(deriveAll ''User "PFUser")
type instance PF User = PFUser
$(mkLabels [''User])

$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost
$(mkLabels [''Post])

instance Index TypeCache User (Domain TypeCache) where index = Suc Zero
instance Index TypeCache Post (Domain TypeCache) where index = Zero

rPosts = NamedLabel lPosts "author_id" -- TODO: should be template haskell.

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
  pFetchHasMany _ _ = error "persist with broken index"

instance (Show a, Regular a,
         GModelName (PF a), 
         GColumns (PF a), 
         GParse (PF a),
         Persist DB' env) => Persist DB' (env, f a) where
  pFetch x@Zero id = DB' $ find (tRefType x) id
  pFetch (Suc y) id = DB' $ unDB' $ pFetch y id
  --pFetchHasMany :: (Regular a, Regular b) => Int -> TRef fam b -> NamedLabel a (HasMany b) -> p fam (RefList fam b)
  pFetchHasMany ix x@(Zero) label = DB' $ fillHasMany ix (key label)
  pFetchHasMany ix  (Suc x) label = DB' $ unDB' $ pFetchHasMany ix x label


example :: CoreData DB' (Domain TypeCache) ()
example = do p     <- fetch tPost 2
             a     <- p ? lAuthor
             posts <- a ? rPosts
             liftIO $ print (count posts)

runExample :: IO ()
runExample = db $ unDB' $ runCoreData example tUser
