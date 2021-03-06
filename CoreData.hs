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
import CoreData.Sqlite

-- Types


-- Example
--
type Domain f = (f Post, (f User, ()))
data User = User {name :: String, password :: String, age :: Int, posts :: Many Post} deriving (Show, Typeable)
data Post = Post {title :: String, body :: String, author :: One User} deriving (Show, Typeable)

$(deriveAll ''User "PFUser")
type instance PF User = PFUser
$(mkLabels [''User])

$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost
$(mkLabels [''Post])

instance Index TypeCache User (Domain TypeCache) where index = Suc Zero
instance Index TypeCache Post (Domain TypeCache) where index = Zero

rPosts = NamedLabel lPosts "author_id" -- TODO: should be template haskell.

allTypes :: IndexList (Domain TypeCache)
allTypes = Cons (tUser) (Cons tPost Nil)
tUser = Suc Zero 
tPost = Zero 


db d = liftIO $ do conn <- connectSqlite3 "coredata.sqlite3"
                   x <- runDB conn d
                   commit conn
                   return x

-- Example

example :: CoreData DB' (Domain TypeCache) ()
example = do p     <- fetch tPost 2 -- fetch a reference to post with id 2
             a     <- p ? lAuthor   -- fetch the author of the post
             posts <- a ? rPosts    -- fetch all the posts by that author
             nm    <- a <@> lName   -- fetch the name of the author
             set a lName "bla"      -- update the name of the author
             liftIO $ print (nm, count posts)

runExample :: IO ()
runExample = do st <- db $ unDB' $ runCoreData example tUser
                print st
                return ()

saveExample = do st <- db $ unDB' $ saveCoreData example tUser allTypes
                 print st
                 return ()
