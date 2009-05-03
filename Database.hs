{-# LANGUAGE FlexibleContexts #-}
module Database where

import Control.Applicative
import Collect
import Records
import Database.Columns
import Database.Values
import Database.Parse
import Database.HDBC
import ModelName
import Relations
import Data.Char (toLower)
import Data.List (intercalate)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3 {- DEBUGGING -})

test = do
  conn <- connectSqlite3 "example.sqlite3"
  user <- find conn (undefined :: User) 1
  fillBelongsTo conn (fromMaybe' "" user) relPost


fillBelongsTo :: ( Rep Parse c, Rep Columns c
                 , Rep ModelName c, Show c
                 ) => Connection 
                   -> m 
                   -> (RBelongsTo m c)
                   -> IO m
fillBelongsTo conn u bt = case btField bt u of
                            BTNotFetched    -> error "fillBelongsTo"
                            BTId x          -> do value <- find conn (fromBelongsTo $ btField bt u) x
                                                  let value' = fromMaybe' "fillBelongsTo" value
                                                  return $ btUpdate bt u $ BTFetched (x, value')
                            x@(BTFetched _) -> return $ btUpdate bt u x
 where fromBelongsTo (BTFetched (x,y)) = y
       fromBelongsTo _                 = error "fromBelongsTo"

fromMaybe' e (Just x) = x
fromMabye' e _        = error e

new :: (Rep Values a, Rep Columns a, Rep ModelName a) => Connection -> a -> IO Int
new conn x = let v = values x
                 c = columns x
                 q = newQuery (tableName x) c
             in case (length v == length c) of
                False -> error "Incorrect instances for Values and Columns"
                True -> do
                     quickQuery' conn q v
                     [[i]] <- quickQuery' conn "SELECT last_insert_rowid() AS [ID]" []
                     return $ fromInteger $ fromSql i

update :: (Rep Values a, Rep Columns a, Rep ModelName a) => Connection -> a -> Int -> IO ()
update conn x i = let v = values x
                      c = columns x
                      q = updateQuery (tableName x) c
                  in case (length v == length c) of
                    False -> error "Incorrect instances for Values and Columns"
                    True -> do quickQuery' conn q (v ++ [toSql i])
                               return ()

find :: (Rep Parse a, Rep Columns a, Rep ModelName a, Show a) => Connection -> a -> Int -> IO (Maybe a)
find conn u i = do let q = findQuery (tableName u) (columns u)
                   res <- map parse <$> (quickQuery' conn q [toSql i])
                   case res of
                        []  -> return Nothing
                        [r] -> return r
                        _   -> return Nothing

tableName x = (map toLower $ modelName x) ++ "s"
                        

newQuery tableName columns = "INSERT INTO " ++ tableName ++ " (" ++ (intercalate ", " columns) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") columns) ++ ")"
updateQuery tableName columns = "UPDATE " ++ tableName ++ " SET (" ++ (intercalate ", " columns) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") columns) ++ ") WHERE id = ?"

findQuery tableName columns = "SELECT " ++ (intercalate ", " columns) ++ " FROM " ++ tableName ++ " WHERE id = ? LIMIT 1"
