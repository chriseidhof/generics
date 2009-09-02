{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Generics.Regular
import Generics.Regular.WebTypes
import Generics.Regular.Happstack
import Generics.Regular.Relations
import Happstack.Server
import Control.Monad (mplus)
import Control.Monad.Trans
import Generics.Regular.Database
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Database.HDBC (commit)

data User = User {name :: String, password :: Password, age :: Int} deriving Show
data Post = Post {title :: String, body :: Textarea, author :: BelongsTo User} deriving Show

$(deriveAll ''User "PFUser")
type instance PF User = PFUser

$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost

mainHandler :: ServerPartT IO Response
mainHandler =   dir "user" (crudHandler (undefined :: TW User) db)
        `mplus` dir "post" (crudHandler (undefined :: TW Post) db)

-- DB stuff
db d = liftIO $ do conn <- connectSqlite3 "happstack.sqlite3"
                   x <- runDB conn d
                   commit conn
                   return x

port_ = 9959

main = do print $ ("running at port", port_)
          simpleHTTP (nullConf {port = port_}) mainHandler
