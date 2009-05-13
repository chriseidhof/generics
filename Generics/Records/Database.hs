{-# LANGUAGE FlexibleContexts #-}
module Generics.Records.Database (
  runDB, new, find, update, DB,
  module Generics.Records.Database.Columns,
  module Generics.Records.Database.Values,
  module Generics.Records.Database.Parse
) where

import Control.Applicative
import Generics.Records
import Generics.Records.Database.Columns hiding (ignore)
import Generics.Records.Database.Values hiding (ignore)
import Generics.Records.Database.Parse
import Database.HDBC
import Generics.Records.ModelName
import Data.Char (toLower)
import Data.List (intercalate)
import Database.HDBC.Sqlite3 (Connection)
import Control.Monad.State

type DB a = StateT Connection IO a

runDB :: Connection -> DB a -> IO a
runDB = flip evalStateT

new :: (Rep Values a, Rep Columns a, Rep ModelName a) => a -> DB Int
new x = let v = values x
            c = columns x
            q = newQuery (tableName x) c
        in case (length v == length c) of
                False -> error $ "Incorrect instances for Values and Columns: " ++ show (v,c)
                True -> do
                     quickQueryS q v
                     [[i]] <- quickQueryS "SELECT last_insert_rowid() AS [ID]" []
                     return $ fromInteger $ fromSql i

update :: (Rep Values a, Rep Columns a, Rep ModelName a) => a -> Int -> DB ()
update x i = let v = values x
                 c = columns x
                 q = updateQuery (tableName x) c
                  in case (length v == length c) of
                    False -> error "Incorrect instances for Values and Columns"
                    True -> do quickQueryS q (v ++ [toSql i])
                               return ()

find :: (Rep Parse a, Rep Columns a, Rep ModelName a, Show a) => a -> Int -> DB (Maybe a)
find u i = do let q = findQuery (tableName u) (columns u)
              res <- map parse <$> (quickQueryS q [toSql i])
              case res of
                        []  -> return Nothing
                        [r] -> return r
                        _   -> return Nothing

tableName x = (map toLower $ modelName x) ++ "s"

quickQueryS :: String -> [SqlValue] -> DB [[SqlValue]]
quickQueryS q v = get >>= \conn -> liftIO (quickQuery' conn q v)
                        

newQuery tableName columns = "INSERT INTO " ++ tableName ++ " (" ++ (intercalate ", " columns) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") columns) ++ ")"
updateQuery tableName columns = "UPDATE " ++ tableName ++ " SET (" ++ (intercalate ", " columns) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") columns) ++ ") WHERE id = ?"

findQuery tableName columns = "SELECT " ++ (intercalate ", " columns) ++ " FROM " ++ tableName ++ " WHERE id = ? LIMIT 1"
