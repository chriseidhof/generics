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

rUser :: (Labeled g) => g String -> g (BelongsTo Post) -> g User
rUser rString rBtPost = ltype epUser (lcon "User" $ lfield "name" rString `lprod` (lfield "password" rString `lprod` (lfield "age" lint `lprod` lfield "post" rBtPost)))

epUser = EP fromUser toUser

fromUser :: User -> (String, (String, (Int, BelongsTo Post)))
fromUser (User x1 x2 x3 x4) = (x1, (x2, (x3, x4)))

toUser (x1, (x2, (x3, x4))) = (User x1 x2 x3 x4)

data Type a = Type
type DB a = IO a

class RBelongsTo model child where
  update :: model -> child -> model

-- fetchRBelongsTo :: (RBelongsTo model child) => Type child -> model -> DB model
-- fetchRBelongsTo = do
--   undefined
