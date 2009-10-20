{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Relations.BelongsTo where

import Control.Applicative
import Database.HDBC (toSql, fromSql)
import Generics.Regular
import Generics.Regular.Database hiding (maybeRead, getOne)
import Generics.Regular.ModelName
import Generics.Regular.Database.Values
import Generics.Regular.Database.Parse
import Generics.Regular.Views
import qualified Text.XHtml.Strict as X
import qualified Generics.Regular.Database.Columns as C

data BelongsTo a = BTId {unBTId :: Int}
 deriving (Show, Read)

instance ParseSql (BelongsTo a) where
  parsef = Just <$> ((BTId . fromSql) <$> getOne) -- Can this be done easier?

instance C.Columns (BelongsTo a) where columns _ l  = [l ++ "_id"]
instance Values    (BelongsTo a) where values = return . toSql . unBTId
