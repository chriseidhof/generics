{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generics.Records.User where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Generics.Records.Database
import Generics.Records.Database.Parse
import Generics.Records.Database.Values
import Generics.Records.ModelName
import Generics.Records
import Generics.Records.Relations
import Generics.Records.Views
import Generics.Records.Forms
import qualified Generics.Records.Database.Columns as C

test = do
  conn <- connectSqlite3 "example.sqlite3"
  runDB conn $ do
    user  <- fromJust <$> find typeUser 1
    user' <- fillBelongsTo user relPost
    return (post user')
-- User datatype

data User = User {name :: String, password :: String, age :: Int, post :: BelongsTo Post}
 deriving (Show)
data Post = Post {title :: String, body :: String, age_ :: Int}
 deriving (Show)

-- TODO: the rest will be generated by TH

typeUser = undefined :: User
typePost = undefined :: Post

instance (Labeled g, Rep g String, Rep g (BelongsTo Post)) => Rep g User where
  rep = rUser rep rep

instance (Labeled g, Rep g String) => Rep g Post where
  rep = rPost rep 

rUser :: (Labeled g) => g String -> g (BelongsTo Post) -> g User
rUser rString rBtPost = ltype epUser (lcon "User" $ lfield "name"     rString 
                                           `lprod` (lfield "password" rString 
                                           `lprod` (lfield "age"      lint 
                                           `lprod` (lfield "post"     rBtPost)))
                                      )

rPost :: (Labeled g) => g String -> g Post
rPost rString       = ltype epPost (lcon "Post" $ lfield "title" rString 
                                         `lprod` ((lfield "body"  rString)
                                         `lprod` (lfield "age"   lint))
                                   )

epUser = EP fromUser toUser
epPost = EP fromPost toPost

fromUser :: User -> (String, (String, (Int, BelongsTo Post)))
fromUser (User x1 x2 x3 x4) = (x1, (x2, (x3, x4)))

fromPost :: Post -> (String, (String, Int))
fromPost (Post x1 x2 x3) = (x1, (x2, x3))


toUser (x1, (x2, (x3, x4))) = (User x1 x2 x3 x4)
toPost (x1, (x2, x3)) = (Post x1 x2 x3)

-- These are the relations
relPost = RBT post $ \u p -> u {post = p}
