{-# LANGUAGE OverloadedStrings #-}

module Cli
  ( main
  ) where

import           Control.Monad (forM)
import qualified Data.ByteString.Char8 as Bs
import           Data.String (IsString(..))
import qualified Data.Time as Time
import qualified Data.List as List
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.Types as PgTypes
import qualified System.Directory as Dir
import qualified System.Exit as Exit

main :: IO ()
main = do
  let
    connectInfo
      = Pg.defaultConnectInfo
      { Pg.connectUser = "test"
      , Pg.connectPassword = "test"
      , Pg.connectDatabase = "test"
      }
  conn <- Pg.connect connectInfo
  executeSqlFile conn "db/000_bootstrap.sql.up"

  activeRev <- getActiveRev conn

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

    newMigrations = getMigrationsToRun activeRev allMigrations

  forM newMigrations $ (\m -> runMigration conn Upgrade m)
  pure ()


getMigrationsToRun :: Rev -> [Migration] -> [Migration]
getMigrationsToRun activeRev allMigrations = undefined


executeSqlFile :: Pg.Connection -> FilePath -> IO ()
executeSqlFile conn filePath = do
  -- Only do this for trusted inputs. This goes around the type safe
  -- API which prevents SQL injections.
  query <- PgTypes.Query <$> Bs.readFile filePath
  Pg.execute_ conn query
  pure ()


runMigration :: Pg.Connection -> EventType -> Migration -> IO NewEvent
runMigration conn eType migration = do
  migrationFile <- mFile eType migration

  let
    newEvent = fromMigration eType migration

  Pg.withTransaction conn $ do
    executeSqlFile conn migrationFile
    insertNewEvent conn newEvent
    markActiveRevision conn migration

  pure newEvent


data Migration
  = Migration
  { mRev :: Rev
  , mDescription :: Bs.ByteString
  , mBaseFile :: FilePath
  } deriving (Show)


instance Pg.ToRow Migration where
  toRow migration = [(Pg.Escape . unRev . mRev) migration]


data Rev = Rev { unRev :: Bs.ByteString }
  deriving (Eq, Show)


instance IsString Rev where
  fromString = Rev . Bs.pack


instance Pg.ToField Rev where
  toField rev = (Pg.Escape . unRev) rev


instance Pg.FromField Rev where
  fromField f dat = Rev <$> Pg.fromField f dat


instance Pg.FromRow Rev where
  fromRow = Pg.field


mFile :: EventType -> Migration -> IO FilePath
mFile eType m = case eType of
  Upgrade -> mUpgradeFile m
  Downgrade -> mDowngradeFile m


mUpgradeFile :: Migration -> IO FilePath
mUpgradeFile migration = do
  let upgradeFilePath = (mBaseFile migration) ++ ".up"
  requireFile upgradeFilePath


mDowngradeFile :: Migration -> IO FilePath
mDowngradeFile migration = do
  let downgradeFilePath = (mBaseFile migration) ++ ".down"
  requireFile downgradeFilePath


requireFile :: FilePath -> IO FilePath
requireFile filePath = do
  res <- Dir.doesFileExist filePath
  if res
    then pure filePath
    else Exit.die $ "file " ++ filePath ++ " required but not found"


data EventType
  = Upgrade
  | Downgrade


instance Pg.ToField EventType where
  toField eventType = case eventType of
    Upgrade -> Pg.Escape "upgrade"
    Downgrade -> Pg.Escape "downgrade"


data NewEvent
  = NewEvent
  { eMigrationRev :: Rev
  , eType :: EventType
  }


instance Pg.ToRow NewEvent where
  toRow e = [Pg.toField (eMigrationRev e), Pg.toField (eType e)]


fromMigration :: EventType -> Migration -> NewEvent
fromMigration eType migration = NewEvent { eMigrationRev = mRev migration, eType = eType}


insertNewEvent :: Pg.Connection -> NewEvent -> IO ()
insertNewEvent conn event = do
  Pg.execute conn "insert into schemactl_events (rev, event_type) values (?, ?)" event
  pure ()


markActiveRevision :: Pg.Connection -> Migration -> IO ()
markActiveRevision conn migration = do
  Pg.execute conn "update schemactl_rev set rev = ? where id = 1" migration
  pure ()


getActiveRev :: Pg.Connection -> IO Rev
getActiveRev conn = do
  res <- Pg.query_ conn "select rev from schemactl_rev limit 1"
  pure (head res) -- Safe, because of our Bootstrap migration
