{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Example where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (mplus)
import Control.Monad.Trans
import Database.HDBC (commit)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Generics.Regular.Database
import Generics.Regular.Formlets
import Generics.Regular.Happstack
import Generics.Regular.Views
import Generics.Regular.WebTypes
import Generics.Regular.Relations
import Happstack.Server
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F

import Example.Model
import Example.Views


mainHandler :: ServerPartT IO Response
mainHandler =   dir "user" (crudHandler (undefined :: TW User) userConfig db)
        `mplus` dir "post" (crudHandler (undefined :: TW Post) postConfig db)

userCreate = Right (return $ User "hi" (Password "defaultPass") 24, userView)
userConfig = defaultConfig {convertView = userView, convertEdit = userView, convertCreate = userCreate}
postCreate = Right (return $ Post "hi" "" BTNotFetched, postView)
postConfig = defaultConfig {convertEdit = postView, convertCreate = postCreate}

-- instances
instance Html    (BelongsTo User) where html = const X.noHtml
--instance Formlet (BelongsTo User) where formlet = pure 

-- DB stuff
db d = liftIO $ do conn <- connectSqlite3 "happstack.sqlite3"
                   x <- runDB conn d
                   commit conn
                   return x

main = do let port_ = 9959
          print $ ("running at port", port_)
          tid <- forkIO $ simpleHTTP (nullConf {port = port_}) mainHandler
          putStrLn $ "Press enter to quit."
          x <- getLine
          killThread tid
          return ()
