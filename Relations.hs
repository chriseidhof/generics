{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Relations where

import Records
import Collect

data BelongsTo a = NotFetched | Fetched (Int, a)

-- User datatype

data User = User {name :: String, password :: String, age :: Int, post :: BelongsTo Post}
data Post = Post {title :: String, body :: String}
