{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Example where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Database.HDBC (fromSql)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Generics.Regular.Relations
import Generics.Regular
import Generics.Regular.Views
import Generics.Regular.Formlets
import Generics.Regular.Database
import Generics.Regular.Database.Columns
import Generics.Regular.Database.Values
import Control.Monad.Trans (liftIO)

test = do
  conn <- connectSqlite3 "example.sqlite3"
  runDB conn $ do
    let userId = 1
    user  <- fromJust <$> find (undefined :: User) userId
    user' <- fillHasMany user userId relPosts
    return user'
    --post  <- fromJust <$> find (undefined :: Post) 1
    --post' <- fillBelongsTo post relAuthor
    --return post'
-- User datatype

newtype Password = Password {unpass :: String}
 deriving Show
instance ParseSql Password where parsef = (Just . Password . fromSql) <$> getOne
instance Columns Password  where columns = const keep

data User = User {name :: String, password :: Password, age :: Int, posts :: HasMany Post}
 deriving (Show)
data Post = Post {title :: String, body :: String, user :: BelongsTo User}
 deriving (Show)

relAuthor = RBT user $ \u p -> u {user = p}
relPosts  = RHM posts $ \u p -> u {posts = p}

$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost

$(deriveAll ''User "PFUser")
type instance PF User = PFUser
