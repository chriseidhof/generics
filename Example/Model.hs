{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
module Example.Model where

import Data.Record.Label
import Generics.Regular
import Generics.Regular.WebTypes
import Generics.Regular.Relations

-- User entity

data User = User {name :: String, password :: Password, age :: Int} deriving Show
$(deriveAll ''User "PFUser")
type instance PF User = PFUser
$(mkLabels [''User])


-- Post entity.
data Post = Post {title :: String, body :: String, author :: BelongsTo User} deriving Show
$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost
$(mkLabels [''Post])
