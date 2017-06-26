{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Util
  ( getDomain
  , redirect301
  ) where

------------
-- Imports
------------

-- Local
import Model (Domain(..))

-- Base
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO)

-- HTTP
import Network.HTTP.Types.Status (status301)

-- Spock
import Network.Wai.Internal (Request, requestHeaderHost)
import Web.Spock (HasSpock , SpockConn , ActionT , runQuery , setStatus , setHeader , html)

-- Text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

------------
-- Helpers
------------

getDomain :: Request -> Maybe Domain
getDomain = (Domain . decodeUtf8 <$>) . requestHeaderHost

redirect301 :: MonadIO m => Text -> ActionT m ()
redirect301 url = do
    setStatus status301
    setHeader "Location" url
    html $ "<html><body>You are being <a src=\"" <> url <> "\">redirected</a>.</body></html>"
