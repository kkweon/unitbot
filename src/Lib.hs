{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  , app
  ) where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


data User = User
  { userId :: Int
  , userFirstName :: String
  , userLastName :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API
   = "users" :> Get '[ JSON] [User] :<|> "healthcheck" :> Get '[ PlainText] Text

startApp :: IO ()
startApp = do
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    run (read port :: Int) app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users :<|> return "Hello World"

users :: [User]
users = [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]
