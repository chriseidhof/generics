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

data To2 mult1 mult2 l r where
  Relation :: Relation (mult1, mult2) l r 
           -> Relation (mult2, mult1) r l
           -> To2 mult1 mult2 l r
mkRelation x y = let x1 = x y1
                     y1 = y x1
                 in Relation x1 y1


data PointersToRelation phi ix where
  NoRelation :: PointersToRelation phi ix
  PointerL   :: PointersToRelation phi ix

class Fam phi => ERModel (phi :: * -> *) env | phi -> env, env -> phi where
  relations2 :: TList phi env

instance ERModel Blog ERRelationsBlog where
  relations2 = TCons User authorPosts $ TCons User authorComments $ TCons Post postComments TNil

type ERRelationsBlog = ((One `To2` Many) Post User
                        ,((One `To2` Many) Comment User
                        ,((One `To2` Many) Comment Post
                        ,()
                        )))
  --relationsForIndex = undefined

relationsForType :: (ERModel phi xs, TEq phi) => phi x -> TList phi (FilterIfTypeEq x xs)
relationsForType ix = filterByType ix relations2

-- data ERRelIndexList a items env where
--   ERILNil :: ERRelIndexList a () env
--   ERILCons :: ERRelIndex phi m1 m2 from a env -> ERRelIndexList a items env -> ERRelIndexList a (To2 m1 m2 from a , items) env
-- 
-- data ERRelIndex (phi :: * -> *) m1 m2 from a env where
--   Left2  :: phi a -> ERRelIndex phi m1 m2 from a (To2 m1 m2 from a, b)
--   Suc2   :: ERRelIndex phi m1 m2 from a env -> ERRelIndex phi m1 m2 from a (b, env)
-- 
-- 
-- lookupRel :: ERRelIndex phi m1 m2 from  a env -> env -> To2 m1 m2 from a
-- lookupRel (Left2 _) env = fst env
-- lookupRel (Suc2 x) env = lookupRel x (snd env)
-- 
-- lookupRel' :: (ERModel phi env) => ERRelIndex phi m1 m2 from a env -> To2 m1 m2 from a
-- lookupRel' ix = lookupRel ix (relations2)
--rel Zero2 = undefined


--lookup' :: nat -> ix -> env -> Index2 nat ix env
--lookup' Zero ix (a, _) = undefined

--newEntity :: (Relations Blog User relations) => User -> relations -> Ref User
--newEntity = undefined

--
--instance ERModel Blog where
--  relations = (authorPosts, (authorComments, (postComments, nil)))

--type instance Relations Blog User     =  ((Many `To` One)  Post) :&: ((Many `To` One) Comment) :&: Nil
--type instance Relations Blog Comment  =  ((One  `To` Many) User) :&: ((One  `To` Many) Post)   :&: Nil
--type instance Relations Blog Post     =  ((One  `To` Many) User) :&: ((Many `To` One) Comment) :&: Nil

authorPosts    = Rel "author" One User `mkRelation` Rel "posts"    Many Post
authorComments = Rel "author" One User `mkRelation` Rel "comments" Many Comment
postComments   = Rel "post"   One Post `mkRelation` Rel "comments" Many Comment

--
  --

exampleUser    = UserC "chris" "test" 24
examplePost    = PostC "fipo" "my first post"
exampleComment = CommentC "a comment!"

--instance HasRelations Blog User where
--  relations = ((To $ snd authorPosts) :&: (To $ snd authorComments) :&: Nil)
--
--instance HasRelations Blog Comment where
--  relations = ((To $ fst authorComments) :&: (To $ fst postComments) :&: Nil)
--
--instance HasRelations Blog Post where
--  relations = ((To $ fst authorPosts) :&: (To $ snd postComments) :&: Nil)

--userPosts    = RZero      :: RelIndex ((Many `To` One) Post)    (Zero)
--userComments = RSuc RZero :: RelIndex ((Many `To` One) Comment) (Suc Zero)

-- example flow
--
--example :: Basil Blog BlogEnv Logger String
--example = do chris  <- new exampleUser (RLNil, (RLNil, ()))
--             name   <- attr chris lName
--             post   <- new examplePost (chris, (RLNil, ()))
--             return name
