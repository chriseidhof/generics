{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances #-}

module CoreData2 where

import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.Base
import Generics.MultiRec.TH
import Basil
import qualified Data.Map as M
import Prelude hiding (log)
import Basil.TContainerList

data User = UserC {name :: String, password :: String, age :: Int} deriving (Show)
data Post = PostC {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show


data Blog :: * -> * where
  User    :: Blog User
  Post    :: Blog Post
  Comment :: Blog Comment

type BlogEnv = (User, (Post, (Comment, ())))

witnesses :: Witnesses Blog BlogEnv
witnesses = WCons (WCons (WCons WNil))

instance EnumTypes Blog BlogEnv where
  allTypes = witnesses
  index User    = Zero
  index Post    = Suc Zero
  index Comment = Suc (Suc Zero)

newtype Logger (phi :: * -> *) a = Log {runLog :: IO a}

log :: Show a => a -> Logger phi ()
log = Log . print

$(mkLabels [''User, ''Post, ''Comment])
$(deriveConstructors [''User, ''Post, ''Comment])
$(deriveSystem ''Blog [''User, ''Post, ''Comment] "PFBlog")
type instance PF Blog = PFBlog

-- TODO: this should be TH as well.
type instance TypeEq User     User    = True 
type instance TypeEq User     Post    = False
type instance TypeEq User     Comment = False
type instance TypeEq Post     User    = False
type instance TypeEq Post     Post    = True 
type instance TypeEq Post     Comment = False
type instance TypeEq Comment  User    = False
type instance TypeEq Comment  Post    = False
type instance TypeEq Comment  Comment = True 

-- TODO: this should be TH as well.
instance TEq Blog where
 tEq User     User    = TTrue 
 tEq User     Post    = TFalse
 tEq User     Comment = TFalse
 tEq Post     User    = TFalse
 tEq Post     Post    = TTrue 
 tEq Post     Comment = TFalse
 tEq Comment  User    = TFalse
 tEq Comment  Post    = TFalse
 tEq Comment  Comment = TTrue 


instance Monad (Logger Blog) where
  return x = Log (return x)
  (>>=) l r = Log (runLog l >>= (fmap runLog r))

instance Persist Logger Blog where
  pFetch tix ix = do log ix
                     return Nothing

instance ERModel Blog ERRelationsBlog where
  relations = TCons Post User authorPosts $ TCons Comment User authorComments $ TCons Comment Post postComments TNil

type ERRelationsBlog = ((One `To` Many) Post User
                       ,((One `To` Many) Comment User
                       ,((One `To` Many) Comment Post
                       ,()
                       )))
  --relationsForIndex = undefined


--newEntity :: (Relations Blog User relations) => User -> relations -> Ref User
--newEntity = undefined

authorPosts    = Rel "author" One User `mkRelation` Rel "posts"    Many Post
authorComments = Rel "author" One User `mkRelation` Rel "comments" Many Comment
postComments   = Rel "post"   One Post `mkRelation` Rel "comments" Many Comment

exampleUser    = UserC "chris" "test" 24
examplePost    = PostC "fipo" "my first post"
exampleComment = CommentC "a comment!"


-- example flow
--
--example :: Basil Blog BlogEnv Logger String
--example = do chris  <- new exampleUser (RLNil, (RLNil, ()))
--             name   <- attr chris lName
--             post   <- new examplePost (chris, (RLNil, ()))
--             return name
