{-# LANGUAGE OverloadedStrings #-}

import Cli hiding (main)
import Test.Hspec

main :: IO ()
main = hspec $ do
  let
    allMigrations
      = [ Migration
          { mRev = "001"
          , mDescription = "add_users"
          , mBaseFile = "db/001_add_users.sql"
          }
        , Migration
          { mRev = "002"
          , mDescription = "add_sessions"
          , mBaseFile = "db/002_add_sessions.sql"
          }
        ]

  describe "getMigrationsToRun" $ do
    it "filters migrations" $ do
      length (getMigrationsToRun (Rev "003") allMigrations) `shouldBe` (0 :: Int)

      length (getMigrationsToRun (Rev "001") allMigrations) `shouldBe` (1 :: Int)
      length (getMigrationsToRun (Rev "002") allMigrations) `shouldBe` (0 :: Int)
      --True `shouldBe` True



















      -- length (getMigrationsToRun (Rev "001") allMigrations) `shouldBe` (1 :: Int)
      -- length (getMigrationsToRun (Rev "002") allMigrations) `shouldBe` (0 :: Int)

      --length (getMigrationsToRun (Rev "003") allMigrations) `shouldBe` (2 :: Int)
