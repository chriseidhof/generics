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

data HasMany a = HMNotFetched 
 deriving (Show, Read)

instance ParseSql (HasMany a) where
  parsef = return (Just HMNotFetched)

instance C.Columns (HasMany a) where columns _ l  = []
instance Values    (HasMany a) where values _ = []

fillHasMany :: ( Regular c, GParse (PF c), GColumns (PF c)
                 , GModelName (PF c), Show c
                 ) => Int
                   -> String
                   -> DB [(Int, c)]
fillHasMany ix bt = findAll undefined [(bt, toSql ix)]
