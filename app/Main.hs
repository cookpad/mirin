{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Data.Aeson (parseJSON, FromJSON, withObject, (.:))

data AppSettings = AppSettings { appPort :: Int }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appPort <- o .: "port"
    return AppSettings{..}

main :: IO ()
main = do
  settings <- liftIO $ loadYamlSettings ["settings.yml"] [] useEnv
  spockCfg <- defaultSpockCfg () PCNoDatabase settings
  runSpock (appPort settings) (spock spockCfg app)

app :: SpockM () () AppSettings ()
app = get root $ text "Hello World!"
