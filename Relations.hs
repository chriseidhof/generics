{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Relations where

import Records
import Collect
import qualified Text.XHtml.Strict as X

data BelongsTo a = BTNotFetched | BTFetched (Int, a)
data HasOne    a = HONotFetched | HOFetched (Int, a)
data HasMany   a = HMNotFetched | HMFetched [(Int, a)]
data HABTM     a = HBNotFetched | HBFetched [(Int, a)]

-- User datatype

data User = User {name :: String, password :: String, age :: Int, post :: BelongsTo Post}
data Post = Post {title :: String, body :: String}

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
