{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Generics.Regular.Views
import Generics.Regular.Formlets
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F


data UserView = UserView {name_ :: String, age_ :: Int} deriving Show
data PostView = PostView {title_ :: String, body_ :: Textarea} deriving Show

data User = User {name :: String, password :: Password, age :: Int} deriving Show
data Post = Post {title :: String, body :: String, author :: BelongsTo User} deriving Show


$(deriveAll ''User "PFUser")
$(deriveAll ''Post "PFPost")
$(deriveAll ''UserView "PFUserView")
$(deriveAll ''PostView "PFPostView")
type instance PF User = PFUser
type instance PF Post = PFPost
type instance PF UserView = PFUserView
type instance PF PostView = PFPostView
$(mkLabels [''Post])
$(mkLabels [''User])

mainHandler :: ServerPartT IO Response
mainHandler =   dir "user" (crudHandler (undefined :: TW User) userConfig db)
        `mplus` dir "post" (crudHandler (undefined :: TW Post) postConfig db)

userView :: User :-> UserView
userView = Wrap $ UserView <$> name_ =&= lName 
                           <*> age_  =&= lAge

(=&=) = cofmap'

postView :: Post :-> PostView
postView = Wrap $ PostView <$> title_  =&= lTitle 
                           <*> (Textarea <$> ((unTextarea . body_) =&= lBody))

userCreate = Right (return $ User "hi" (Password "defaultPass") 24, userView)
postCreate = Right (return $ Post "hi" "" BTNotFetched, postView)
userConfig = defaultConfig {convertView = userView, convertEdit = userView, convertCreate = userCreate}
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
          simpleHTTP (nullConf {port = port_}) mainHandler
