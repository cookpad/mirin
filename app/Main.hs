{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Data.Aeson (parseJSON, FromJSON, withObject, (.:))
import Database.Persist.TH (persistLowerCase, share, mkPersist, sqlSettings, mkMigrate)
import Database.Persist.MySQL ( MySQLConf
                              , createMySQLPool
                              , SqlBackend
                              , myConnInfo
                              , myPoolSize
                              , runSqlPool
                              , printMigration
                              )
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)

data AppSettings = AppSettings { appPort :: Int
                               , appDatabaseConf :: MySQLConf
                               }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \json -> AppSettings
    <$> json .: "port"
    <*> json .: "database"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Redirect sql=redirects
  domain Text
  originalPath Text
  destinationPath Text
  DomainRedirect domain originalPath
|]

main :: IO ()
main = do
  settings <- loadYamlSettings ["settings.yml"] [] useEnv
  pool <- runStdoutLoggingT $ createMySQLPool
    (myConnInfo $ appDatabaseConf settings)
    (myPoolSize $ appDatabaseConf settings)
  spockCfg <- defaultSpockCfg () (PCPool pool) settings
  runStdoutLoggingT $ runSqlPool (printMigration migrateAll) pool
  runSpock (appPort settings) (spock spockCfg app)

app :: SpockM SqlBackend () AppSettings ()
app = get root $ text "Hello World!"
