{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model
  ( runSQL
  , createLoggingPool
  , printMigrations
  , getRedirection
  , Domain(..)
  , Redirect(..)
  ) where
------------
-- Imports
------------

-- Base
import Data.Text (Text)

-- Logging
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

-- Mysql
import Database.Persist.MySQL

-- Persistent
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

-- Spock
import Web.Spock ( HasSpock , SpockConn , runQuery)
import Data.Pool (Pool)

-- Transformers
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)

------------
-- Types
------------

type DB = ReaderT SqlBackend (LoggingT (ResourceT IO))
type UrlPath = Text
newtype Domain = Domain { domainText :: Text }

------------
-- Model
------------

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Redirect sql=redirects
      domain Text
      originalPath Text
      destinationPath Text
      DomainRedirect domain originalPath
  |]

------------
-- Helpers
------------

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT (ResourceT IO)) a -> m a
runSQL action = runQuery $ \conn -> runResourceT $ runStdoutLoggingT $ runSqlConn action conn

createLoggingPool :: MySQLConf -> IO (Pool SqlBackend)
createLoggingPool conf = runStdoutLoggingT $ createMySQLPool (myConnInfo conf) (myPoolSize conf)

printMigrations :: Pool SqlBackend -> IO ()
printMigrations pool = runStdoutLoggingT $ runSqlPool (printMigration migrateAll) pool

getRedirection :: UrlPath -> Domain -> DB (Maybe (Entity Redirect))
getRedirection path domain = selectFirst cond []
  where cond = [ RedirectDomain ==. domainText domain
               , RedirectOriginalPath ==. path
               ]
