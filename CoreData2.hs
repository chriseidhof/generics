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
module CoreData2 where

import Data.Record.Label (mkLabels, label)
import Generics.MultiRec.Base
import Generics.MultiRec.TH
import Basil
import qualified Data.Map as M
import Prelude hiding (log)

data User = UserC {name :: String, password :: String, age :: Int} deriving (Show)
data Post = PostC {title :: String, body :: String} deriving (Show)
data Comment = CommentC {text :: String} deriving Show

exampleUser    = UserC "chris" "test" 24
examplePost    = PostC "fipo" "my first post"
exampleComment = CommentC "a comment!"

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

instance Monad (Logger Blog) where
  return x = Log (return x)
  (>>=) l r = Log (runLog l >>= (fmap runLog r))

instance Persist Logger Blog where
  pFetch tix ix = do log ix
                     return Nothing

--newEntity :: (Relations Blog User relations) => User -> relations -> Ref User
--newEntity = undefined
--
type instance Relations Blog User     =  ((Many `To` One)  Post) :&: ((Many `To` One) Comment) :&: Nil
type instance Relations Blog Comment  =  ((One  `To` Many) User) :&: ((One  `To` Many) Post)   :&: Nil
type instance Relations Blog Post     =  ((One  `To` Many) User) :&: ((Many `To` One) Comment) :&: Nil

authorPosts = let author = Rel "author" One  Post User posts
                  posts  = Rel "posts"  Many User Post author
              in (author, posts)
authorComments = let author   = Rel "author"   One Comment User comments
                     comments = Rel "comments" Many User Comment author
                 in (author, comments)
postComments   = let post     = Rel "post"     One  Comment Post comments
                     comments = Rel "comments" Many Post Comment post
                 in (post, comments)
--
  --


instance HasRelations Blog User where
  relations = ((To $ snd authorPosts) :&: (To $ snd authorComments) :&: Nil)

instance HasRelations Blog Comment where
  relations = ((To $ fst authorComments) :&: (To $ fst postComments) :&: Nil)

instance HasRelations Blog Post where
  relations = ((To $ fst authorPosts) :&: (To $ snd postComments) :&: Nil)

userPosts    = RZero      :: RelIndex ((Many `To` One) Post)    (Zero)
userComments = RSuc RZero :: RelIndex ((Many `To` One) Comment) (Suc Zero)

-- example flow

example :: Basil Blog BlogEnv Logger String
example = do chris  <- new exampleUser (RLNil, (RLNil, ()))
             name   <- attr chris lName
             post   <- new examplePost (chris, (RLNil, ()))
             return name
