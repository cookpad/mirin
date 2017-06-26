{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

------------
-- Imports
------------

-- Base
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

-- MySQL
import Database.Persist.MySQL (SqlBackend, entityVal)

-- Spock
import Web.Spock (SpockM , spock , runSpock , get , root , html , wildcard , getState , request)
import Web.Spock.Config (defaultSpockCfg, PoolOrConn(..))

-- Transformers
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

-- Local
import Util (getDomain, redirect301)
import Model (runSQL, createLoggingPool, printMigrations, getRedirection, redirectDestinationPath)
import Settings (loadSettings, AppSettings(..))

------------
-- App
------------

main :: IO ()
main = do
  settings@AppSettings{..} <- loadSettings
  pool <- createLoggingPool appDatabaseConf
  spockCfg <- defaultSpockCfg () (PCPool pool) settings
  printMigrations pool
  runSpock appPort (spock spockCfg app)

app :: SpockM SqlBackend () AppSettings ()
app = get wildcard $ \path -> do
  rootUrl <- appBase <$> getState
  mpath <- runMaybeT $ do
    domain <- MaybeT (getDomain <$> request)
    redirection <- MaybeT (runSQL $ getRedirection path domain)
    return . redirectDestinationPath . entityVal $ redirection
  redirect301 $ rootUrl <> fromMaybe "" mpath
