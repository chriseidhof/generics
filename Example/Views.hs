{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Example.Views where

import Control.Applicative
import Data.Record.Label
import Example.Model
import Generics.Regular
import Generics.Regular.WebTypes
import Generics.Regular.Relations

-- User views
data UserView = UserView {name_ :: String, age_ :: Int} deriving Show
$(deriveAll ''UserView "PFUserView")
type instance PF UserView = PFUserView

userView :: User :-> UserView
userView = Label $ UserView <$> name_ `for` lName 
                            <*> age_  `for` lAge

-- Post views
data PostView = PostView {title_ :: String, body_ :: Textarea} deriving Show
-- $(deriveAll ''PostView "PFPostView")
-- type instance PF PostView = PFPostView
postView :: Post :-> PostView
postView = Label $ PostView <$> title_  `for` lTitle 
                            <*> (Textarea <$> ((unTextarea . body_) `for` lBody))
