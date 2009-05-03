{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Relations where

import Records
import Collect
import qualified Text.XHtml.Strict as X

data BelongsTo a = BTNotFetched | BTId Int | BTFetched (Int, a)
 deriving Show
data HasOne    a = HONotFetched | HOId Int | HOFetched (Int, a)
data HasMany   a = HMNotFetched | HMId Int | HMFetched [(Int, a)]
data HABTM     a = HBNotFetched | HBId Int | HBFetched [(Int, a)]

-- User datatype

data User = User {name :: String, password :: String, age :: Int, post :: BelongsTo Post}
 deriving (Show)
data Post = Post {title :: String, body :: String}
 deriving (Show)

instance (Rep View (BelongsTo a)) where
  rep = View $ const X.noHtml

chris = User "chris" "test" 24 BTNotFetched

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

data Type a = Type
type DB a = IO a

-- Existential?
data RBelongsTo a b = RBT { btField  :: a -> BelongsTo b
                          , btUpdate :: a -> BelongsTo b -> a
                          }
