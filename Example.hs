{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Generics.Records.User where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
-- import Generics.Records.ModelName
-- import Generics.Records
import Generics.Regular.Relations
-- import qualified Generics.Records.Database.Columns as C
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
    post  <- fromJust <$> find (undefined :: Post) 1
    post' <- fillBelongsTo post relAuthor
    return post'
-- User datatype

data User = User {name :: String, password :: String, age :: Int}
 deriving (Show)
data Post = Post {title :: String, body :: String, user :: BelongsTo User}
 deriving (Show)

relAuthor = RBT user $ \u p -> u {user = p}

$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost

$(deriveAll ''User "PFUser")
type instance PF User = PFUser
