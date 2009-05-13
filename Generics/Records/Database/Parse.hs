{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generics.Records.Database.Parse where

import Control.Applicative
import Control.Monad.State
import Database.HDBC
import Generics.Records

instance Applicative (State s) where
  pure = return
  (<*>) = ap

data Parse a = Parse {toParse :: State [SqlValue] (Maybe a)}
instance Labeled Parse where
  lconstant    = parseUsingRead
  lprod ra rb  = Parse $ do l <- toParse ra 
                            r <- toParse rb
                            return $ (,) <$> l <*> r
  lfield l r   = Parse $ toParse r
  lcon   l r   = Parse $ toParse r
  ltype ep r   = Parse $ (fmap $ to ep) <$> toParse r

instance Rep Parse String where
  rep = Parse $ fromSql <$> getOne

getOne :: State [SqlValue] SqlValue
getOne = do x <- gets $ head' ("Database doesn't match defined schema.")
            modify tail
            return x

parse :: (Rep Parse a) => [SqlValue] -> Maybe a
parse = evalState (toParse rep)

parseUsingRead :: (Read a) => Parse a
parseUsingRead = Parse $ maybeRead <$> getOne


-- TODO: refactor
maybeRead :: Read a => SqlValue -> Maybe a
maybeRead SqlNull = Nothing
maybeRead x       = (Just . read . fromSql) x

head' _ (x:xs) = x
head' e _      = error e
