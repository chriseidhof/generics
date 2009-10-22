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

import Generics.MultiRec.Base
import Generics.MultiRec.TH
import Basil.Core
import qualified Data.Map as M

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

authorPosts = let author = Rel "author" One  Post User posts
                  posts  = Rel "posts" Many User Post author
              in (author, posts)
authorComments = let author   = Rel "author"   One Comment User comments
                     comments = Rel "comments" Many User Comment author
                 in (author, comments)
postComments   = let post     = Rel "post"     One  Comment Post comments
                     comments = Rel "comments" Many Post Comment post
                 in (post, comments)

witnesses :: Witnesses Blog (User, (Post, (Comment, ())))
witnesses = WCons (WCons (WCons WNil))

instance EnumTypes Blog (User, (Post, (Comment, ()))) where
  allTypes = witnesses


$(deriveConstructors [''User, ''Post, ''Comment])
$(deriveSystem ''Blog [''User, ''Post, ''Comment] "PFBlog")
type instance PF Blog = PFBlog

--newEntity :: (Relations Blog User relations) => User -> relations -> Ref User
--newEntity = undefined
--
type instance Relations Blog User     =  ((Many `To` One) Post) 
                                     :&: ((Many `To` One) Comment) 
                                     :&: Nil
type instance Relations Blog Comment = ((One  `To` Many) User) 
                                    :&: ((One `To` Many) Post) 
                                    :&: Nil
type instance Relations Blog Post    =  ((One `To` Many) User) 
                                    :&: ((One `To` Many) Post) 
                                    :&: Nil
--
  --


instance HasRelations Blog User where
  relations = ((To $ snd authorPosts) :&: (To $ snd authorComments) :&: Nil)

instance HasRelations Blog Comment where
  relations = ((To $ fst authorComments) :&: (To $ fst postComments) :&: Nil)


--data Zero
--data Suc a
--
--type family Index 
--type instance Index Zero 


userPosts :: RelIndex ((Many `To` One) Post) Zero
userPosts    = RZero
userComments :: RelIndex ((Many `To` One) Comment) (Suc Zero)
userComments = RSuc RZero



--index :: (El phi ix, HasRelations phi ix) => Rel phi ix -> Index (Relations phi ix) -> ix
-- new :: (El Blog ix, HasRelations phi ix) => ix -> (Relations Blog ix) Blog ix -> ix
-- new = undefined
