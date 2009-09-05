{-# LANGUAGE FlexibleContexts #-}
module Generics.Regular.Database (
  runDB, new, find, update, findAll, createTable, DB,
  module Generics.Regular.Database.Columns,
  module Generics.Regular.Database.Values,
  module Generics.Regular.Database.Parse
) where

import Control.Applicative
import Generics.Regular
import Generics.Regular.Database.Columns
import Generics.Regular.Database.Values
import Generics.Regular.Database.Parse
import Database.HDBC
import Generics.Regular.ModelName
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Database.HDBC.Sqlite3 (Connection)
import Control.Monad.State
import Control.Monad.Trans (liftIO)

type DB a = StateT Connection IO a

runDB :: Connection -> DB a -> IO a
runDB = flip evalStateT

new :: (Regular a, GValues (PF a), GColumns (PF a), GModelName (PF a)) => a -> DB Int
new x = let v = gvalues x
            c = gtocolumns x
            q = newQuery (tableName $ from x) c
        in case (length v == length c) of
                False -> error $ "Incorrect instances for Values and Columns: " ++ show (v,c)
                True -> do
                     quickQueryS q v
                     [[i]] <- quickQueryS "SELECT last_insert_rowid() AS [ID]" []
                     return $ fromInteger $ fromSql i

update :: (Regular a, GValues (PF a), GColumns (PF a), GModelName (PF a)) => a -> Int -> DB ()
update x i = let v = gvalues x
                 c = gtocolumns x
                 q = updateQuery (tableName $ from x) c
                  in case (length v == length c) of
                    False -> error "Incorrect instances for Values and Columns"
                    True -> do quickQueryS q (v ++ [toSql i])
                               return ()

find :: (Regular a, GParse (PF a), GColumns (PF a), GModelName (PF a), Show a) => a -> Int -> DB (Maybe a)
find u i = do let q = findQuery (tableName $ from u) (gtocolumns u)
              res <- map parse <$> (quickQueryS q [toSql i])
              case res of
                        []  -> return Nothing
                        [r] -> return r
                        _   -> return Nothing

findAll :: (Regular a, GParse (PF a), GColumns (PF a), GModelName (PF a), Show a) => a -> [(String, SqlValue)] -> DB [(Int, a)]
findAll u w = do let q  = findAllQuery (tableName $ from u) ("id" : (gtocolumns u)) w
                 x <- (quickQueryS q $ map snd w)
                 return $ catMaybes $ map (evalState parse') x
    where parse' :: (Regular a, GParse (PF a)) => Parser (Int, a)
          parse' = do Just i <- parseUsingRead
                      x <- gparse
                      return $ fmap (\v -> (i,v)) x

createTable :: (Regular a, GColumns (PF a), GModelName (PF a), Show a) => a -> DB ()
createTable u = do let q = createTableQuery (tableName $ from u) (gtocolumns u)
                   quickQueryS q []
                   return ()


createTableQuery tableName columns = "CREATE TABLE " ++ tableName ++ " ( id integer primary key, " ++  (intercalate ", " columns) ++ " )"

tableName x = (map toLower $ gmodelName x) ++ "s"

quickQueryS :: String -> [SqlValue] -> DB [[SqlValue]]
quickQueryS q v = get >>= \conn -> liftIO (quickQuery' conn q v)
                        

newQuery tableName columns = "INSERT INTO " ++ tableName ++ " (" ++ (intercalate ", " columns) ++ ") VALUES (" ++ (intercalate ", " $ map (const "?") columns) ++ ")"
updateQuery tableName columns = "UPDATE " ++ tableName ++ " SET " ++ (intercalate ", " changes) ++ " WHERE id = ?"
                                                                                                                            where changes = map (++ " = ?") columns
                                                                                                                                                                              

findQuery tableName columns = "SELECT " ++ (intercalate ", " columns) ++ " FROM " ++ tableName ++ " WHERE id = ? LIMIT 1"

findAllQuery tableName columns fields = "SELECT " ++ (intercalate ", " columns) ++ " FROM " ++ tableName ++ (conds fields)
  where conds []      = ""
        conds (x:xs)  = " WHERE " ++ (intercalate " AND " $ map ((++ " = ?") . fst) (x:xs))
