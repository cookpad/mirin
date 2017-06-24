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

-- Base
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

-- Aeson
import Data.Aeson ((.:), FromJSON)
import qualified Data.Aeson as A

-- HTTP
import Network.HTTP.Types.Status (status301)

-- Logging
import Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Logger as L

-- Mysql
import Database.Persist.MySQL (MySQLConf, Entity(..), (==.))
import qualified Database.Persist.MySQL as MS

-- Persistent
import qualified Database.Persist.TH as P
 
-- Spock
import Network.Wai.Internal (Request, requestHeaderHost)
import Web.Spock ( SpockM
                 , HasSpock
                 , SpockConn
                 , spock
                 , runSpock
                 , get
                 , root
                 , html
                 , wildcard
                 , ActionT
                 , runQuery
                 , setStatus
                 , setHeader
                 , getState
                 , request
                 )
import Web.Spock.Config (defaultSpockCfg, PoolOrConn(..))

-- Text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

-- Transformers
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)

-- Yaml
import qualified Data.Yaml.Config as Y


------------
-- Settings
------------
data AppSettings = AppSettings { appPort :: Int
                               , appDatabaseConf :: MySQLConf
                               , appBase :: Text
                               }

instance FromJSON AppSettings where
  parseJSON = A.withObject "AppSettings" $ \json -> AppSettings
    <$> json .: "port"
    <*> json .: "database"
    <*> json .: "base-url"


------------
-- Database
------------
type DB = ReaderT MS.SqlBackend (LoggingT (ResourceT IO))

runSQL :: (HasSpock m, SpockConn m ~ MS.SqlBackend) => MS.SqlPersistT (LoggingT (ResourceT IO)) a -> m a
runSQL action = runQuery $ \conn -> runResourceT $ L.runStdoutLoggingT $ MS.runSqlConn action conn

P.share
  [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"]
  [P.persistLowerCase|
    Redirect sql=redirects
      domain Text
      originalPath Text
      destinationPath Text
      DomainRedirect domain originalPath
  |]

getRedirection :: UrlPath -> Domain -> DB (Maybe (Entity Redirect))
getRedirection path domain = MS.selectFirst cond []
  where cond = [ RedirectDomain ==. domainText domain
               , RedirectOriginalPath ==. path
               ]


------------
-- Spock
------------
type UrlPath = Text
newtype Domain = Domain { domainText :: Text }

redirect301 :: MonadIO m => Text -> ActionT m ()
redirect301 url = do
    setStatus status301
    setHeader "Location" url
    html $ "<html><body>You are being <a src=\"" <> url <> "\">redirected</a>.</body></html>"

getDomain :: Request -> Maybe Domain
getDomain = (Domain . decodeUtf8 <$>) . requestHeaderHost

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
app = get wildcard $ \path -> do
  rootUrl <- appBase <$> getState
  mpath <- runMaybeT $ do
    domain <- MaybeT (getDomain <$> request)
    redirection <- MaybeT (runSQL $ getRedirection path domain)
    return . redirectDestinationPath . entityVal $ redirection
  redirect301 $ rootUrl <> fromMaybe "" mpath
