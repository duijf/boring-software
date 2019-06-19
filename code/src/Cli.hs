{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cli
  ( main
  ) where

import qualified Data.ByteString as Bs
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Types as PgTypes


main :: IO ()
main = do
  let
    connInfo
      = Pg.defaultConnectInfo
      { Pg.connectUser = "test"
      , Pg.connectPassword = "test"
      , Pg.connectDatabase = "test" }

  conn <- Pg.connect connInfo

  executeSqlFile conn "db/000_bootstrap.sql.up"

  pure ()


executeSqlFile :: Pg.Connection -> FilePath -> IO ()
executeSqlFile conn filePath = do
  contents <- PgTypes.Query <$> Bs.readFile filePath
  Pg.execute_ conn contents
  pure ()
