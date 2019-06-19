{-# LANGUAGE OverloadedStrings #-}

module Cli
  ( main
  ) where

import qualified Database.PostgreSQL.Simple as Pg

main :: IO ()
main = do
  let
    connInfo
      = Pg.defaultConnectInfo
      { Pg.connectUser = "test"
      , Pg.connectPassword = "test"
      , Pg.connectDatabase = "test" }

  conn <- Pg.connect connInfo

  putStrLn "hi"

  pure ()
