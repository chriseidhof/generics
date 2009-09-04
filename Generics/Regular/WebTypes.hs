module Generics.Regular.WebTypes where

import Control.Applicative
import Database.HDBC (toSql)
import Generics.Regular
import Generics.Regular.Database.Parse
import Generics.Regular.Database.Columns
import Generics.Regular.Database.Values
import Generics.Regular.Views
import Generics.Regular.Formlets
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F

newtype Password = Password {unpass :: String} deriving Show

instance ParseSql Password where parsef = fmap Password <$> getString
instance Columns  Password where columns   = const keep
instance Values   Password where values    = return . toSql . unpass
instance Html     Password where html      = const X.noHtml
instance Formlet  Password where formlet x = Password <$> F.password (unpass <$> x)

newtype Textarea = Textarea {unTextarea :: String} deriving Show
instance Html     Textarea where html    t = X.thediv X.<< (unTextarea t)
instance Formlet  Textarea where formlet x = Textarea <$> F.textarea (Just 10) (Just 80) (unTextarea <$> x)
