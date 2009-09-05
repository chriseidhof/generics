{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Relations.HasMany where

import Control.Applicative
import Database.HDBC (toSql)
import Generics.Regular
import Generics.Regular.Database hiding (maybeRead, getOne)
import Generics.Regular.ModelName
import Generics.Regular.Database.Values
import Generics.Regular.Database.Parse
import Generics.Regular.Views
import Generics.Regular.Formlets
import qualified Text.XHtml.Strict as X
import qualified Generics.Regular.Database.Columns as C
import qualified Text.XHtml.Strict.Formlets as F

data HasMany a = HMNotFetched | HMFetched [(Int, a)]
 deriving (Show, Read)

instance ParseSql (HasMany a) where
  parsef = return (Just HMNotFetched)

instance C.Columns (HasMany a) where columns _ l  = []
instance Values    (HasMany a) where values _ = []

-- TODO: should be fclenses arrow 
data RHasMany a b = RHM { hmField  :: a -> HasMany b
                        , hmForeignKey :: String
                        , hmUpdate :: a -> HasMany b -> a
                        }

fillHasMany :: ( Regular c, GParse (PF c), GColumns (PF c)
                 , GModelName (PF c), Show c
                 ) => m 
                   -> Int
                   -> (RHasMany m c)
                   -> DB m
fillHasMany u ix bt = case hmField bt u of
                        HMNotFetched -> do rels <- findAll (hmType bt) [(hmForeignKey bt, toSql ix)]
                                           return $ hmUpdate bt u $ HMFetched rels
                        HMFetched _  -> return u

hmType :: RHasMany m c -> c
hmType = undefined
