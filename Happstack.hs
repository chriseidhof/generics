{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Example where

import Control.Applicative
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
import Data.Record.Label


data UserView = UserView {name_ :: String, age_ :: Int} deriving Show

data User = User {name :: String, password :: Password, age :: Int} deriving Show
data Post = Post {title :: String, body :: Textarea, author :: BelongsTo User} deriving Show


$(deriveAll ''UserView "PFUserView")
type instance PF UserView = PFUserView
$(deriveAll ''User "PFUser")
type instance PF User = PFUser
$(mkLabels [''User])
$(deriveAll ''Post "PFPost")
type instance PF Post = PFPost
$(mkLabels [''Post])

userView :: User :-> UserView
userView = Wrap $ UserView <$> cofmap' name_ lName <*> cofmap' age_ lAge

userConfig = defaultConfig {convertView = userView, convertEdit = userView}


mainHandler :: ServerPartT IO Response
mainHandler =   dir "user" (crudHandler (undefined :: TW User) userConfig db)
        `mplus` dir "post" (crudHandler (undefined :: TW Post) defaultConfig db)

-- DB stuff
db d = liftIO $ do conn <- connectSqlite3 "happstack.sqlite3"
                   x <- runDB conn d
                   commit conn
                   return x

main = do let port_ = 9959
          print $ ("running at port", port_)
          simpleHTTP (nullConf {port = port_}) mainHandler
