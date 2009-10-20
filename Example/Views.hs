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

(=&=) = cofmap'

-- User views
data UserView = UserView {name_ :: String, age_ :: Int} deriving Show
$(deriveAll ''UserView "PFUserView")
type instance PF UserView = PFUserView

userView :: User :-> UserView
userView = Wrap $ UserView <$> name_ =&= lName 
                           <*> age_  =&= lAge

-- Post views
data PostView = PostView {title_ :: String, body_ :: Textarea} deriving Show
-- $(deriveAll ''PostView "PFPostView")
-- type instance PF PostView = PFPostView
postView :: Post :-> PostView
postView = Wrap $ PostView <$> title_  =&= lTitle 
                           <*> (Textarea <$> ((unTextarea . body_) =&= lBody))
