{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module User where

import Records
import Relations
import Database
import Database.Columns
import Database.Values
import Database.Parse
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)

test = do
  conn <- connectSqlite3 "example.sqlite3"
  user <- find conn (undefined :: User) 1
  fillBelongsTo conn (fromMaybe' "" user) relPost
-- User datatype

data User = User {name :: String, password :: String, age :: Int, post :: BelongsTo Post}
 deriving (Show)
data Post = Post {title :: String, body :: String}
 deriving (Show)

-- TODO: the rest should be done by TH

instance (Labeled g, Rep g String, Rep g (BelongsTo Post)) => Rep g User where
  rep = rUser rep rep

instance (Labeled g, Rep g String) => Rep g Post where
  rep = rPost rep

relPost = RBT post $ \u p -> u {post = p}

rUser :: (Labeled g) => g String -> g (BelongsTo Post) -> g User
rUser rString rBtPost = ltype epUser (lcon "User" $ lfield "name" rString 
                                           `lprod` (lfield "password" rString 
                                           `lprod` (lfield "age" lint `lprod` lfield "post" rBtPost))
                                      )

rPost :: (Labeled g) => g String -> g Post
rPost rString = ltype epPost (lcon "Post" $ lfield "title" rString 
                                   `lprod` (lfield "body"  rString)
                             )

epUser = EP fromUser toUser
epPost = EP fromPost toPost

fromUser :: User -> (String, (String, (Int, BelongsTo Post)))
fromUser (User x1 x2 x3 x4) = (x1, (x2, (x3, x4)))

fromPost :: Post -> (String, String)
fromPost (Post x1 x2) = (x1, x2)


toUser (x1, (x2, (x3, x4))) = (User x1 x2 x3 x4)
toPost (x1, x2) = (Post x1 x2)
