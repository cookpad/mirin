{-# LANGUAGE OverloadedStrings #-}
module Settings
  ( AppSettings(..)
  , loadSettings
  )where

------------
-- Imports
------------

-- Aeson
import Data.Aeson ((.:), FromJSON, parseJSON, withObject)

-- MySQL
import Database.Persist.MySQL (MySQLConf)

-- Text
import Data.Text (Text)

-- Yaml
import Data.Yaml.Config (loadYamlSettings, useEnv)

------------
-- Settings
------------

data AppSettings = AppSettings { appPort :: Int
                               , appDatabaseConf :: MySQLConf
                               , appBase :: Text
                               }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \json -> AppSettings
    <$> json .: "port"
    <*> json .: "database"
    <*> json .: "base-url"

------------
-- Helpers
------------

loadSettings :: IO AppSettings
loadSettings = loadYamlSettings ["settings.yml"] [] useEnv
