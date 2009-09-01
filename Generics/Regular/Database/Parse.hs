{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Database.Parse where

import Control.Applicative
import Control.Monad.State
import Database.HDBC
import Generics.Regular

type Parser a = State [SqlValue] (Maybe a)

class ParseSql a where
  parse :: Parser a

class GParse f where
  gparsef :: Parser a -> Parser (f a)

instance GParse I where
  gparsef f = fmap I <$> f

instance (Constructor c, GParse f) => GParse (C c f) where
  gparsef f = fmap C <$> gparsef f

instance ParseSql a => GParse (K a) where
  gparsef _ = fmap K <$> parse

instance (GParse f, GParse g) => GParse (f :*: g) where
  gparsef f = do l <- gparsef f 
                 r <- gparsef f
                 return $ (:*:) <$> l <*> r

instance (Selector s, GParse f) => GParse (S s f) where
  gparsef f = gparsef f

getOne :: State [SqlValue] SqlValue
getOne = do x <- gets $ head' ("Database doesn't match defined schema.")
            modify tail
            return x

parseUsingRead :: (Read a) => Parser a
parseUsingRead = maybeRead <$> getOne

-- TODO: refactor
maybeRead :: Read a => SqlValue -> Maybe a
maybeRead SqlNull = Nothing
maybeRead x       = (Just . read . fromSql) x

head' _ (x:xs) = x
head' e _      = error e
