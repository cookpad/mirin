{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

------------
-- Imports
------------

-- Aeson
import Data.Aeson ((.:), FromJSON)
import qualified Data.Aeson as A

-- Logging
import qualified Control.Monad.Logger as L

-- Mysql
import Database.Persist.MySQL (MySQLConf)
import qualified Database.Persist.MySQL as MS

-- Persistent
import qualified Database.Persist.TH as P
 
-- Spock
import Web.Spock (SpockM, spock, runSpock, get, root, text)
import Web.Spock.Config (defaultSpockCfg, PoolOrConn(..))

-- Text
import Data.Text (Text)

-- Yaml
import qualified Data.Yaml.Config as Y


------------
-- Settings
------------
data AppSettings = AppSettings { appPort :: Int
                               , appDatabaseConf :: MySQLConf
                               }

instance FromJSON AppSettings where
  parseJSON = A.withObject "AppSettings" $ \json -> AppSettings
    <$> json .: "port"
    <*> json .: "database"


------------
-- Database
------------
P.share
  [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"]
  [P.persistLowerCase|
    Redirect sql=redirects
      domain Text
      originalPath Text
      destinationPath Text
      DomainRedirect domain originalPath
  |]


------------
-- Spock
------------
main :: IO ()
main = do
  settings@AppSettings{..} <- Y.loadYamlSettings ["settings.yml"] [] Y.useEnv
  pool <- L.runStdoutLoggingT $ MS.createMySQLPool
    (MS.myConnInfo appDatabaseConf)
    (MS.myPoolSize appDatabaseConf)
  spockCfg <- defaultSpockCfg () (PCPool pool) settings
  L.runStdoutLoggingT $ MS.runSqlPool (MS.printMigration migrateAll) pool
  runSpock appPort (spock spockCfg app)

app :: SpockM MS.SqlBackend () AppSettings ()
app = get root $ text "Hello World!"
