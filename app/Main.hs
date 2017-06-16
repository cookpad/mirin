{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Data.Aeson (parseJSON, FromJSON, withObject, (.:))
import Database.Persist.MySQL (MySQLConf, createMySQLPool, SqlBackend, myConnInfo, myPoolSize)
import Control.Monad.Logger (runStdoutLoggingT)

data AppSettings = AppSettings { appPort :: Int
                               , appDatabaseConf :: MySQLConf
                               }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \json -> AppSettings
    <$> json .: "port"
    <*> json .: "database"

main :: IO ()
main = do
  settings <- liftIO $ loadYamlSettings ["settings.yml"] [] useEnv
  pool <- runStdoutLoggingT $ createMySQLPool
    (myConnInfo $ appDatabaseConf settings)
    (myPoolSize $ appDatabaseConf settings)
  spockCfg <- defaultSpockCfg () (PCPool pool) settings
  runSpock (appPort settings) (spock spockCfg app)

app :: SpockM SqlBackend () AppSettings ()
app = get root $ text "Hello World!"
