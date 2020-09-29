{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App
  ( startApp
  , app
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Lens                   ( (.~)
                                                , (^.)
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( liftIO )
import           Conversion                     ( convert )
import           Conversion.Text
import           Data.Aeson
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text
import           Data.Text.Encoding             ( encodeUtf8 )
import           HTMLEntities.Decoder           ( htmlEncodedText )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Servant
import           System.Environment             ( getEnv
                                                , lookupEnv
                                                )

import qualified Data.Quantities               as Quantities
import qualified Data.Text.IO                  as TIO
import qualified Network.Wreq                  as Wreq

import           Slack.Model                    ( SlackBotMessage(..)
                                                , SlackMessageEvent(..)
                                                , SlackRequest(..)
                                                , SlackResponse(..)
                                                )

type API
  = "api" :> "slack" :> ReqBody '[ JSON] SlackRequest :> Post '[ JSON] SlackResponse :<|> "healthcheck" :> Get '[ PlainText] Text

startApp :: IO ()
startApp = do
  token   <- getEnv "TOKEN"
  portStr <- fromMaybe "80" <$> lookupEnv "PORT"
  let portInt = read portStr :: Int
  withStdoutLogger $ \aplogger -> do
    let settings = setPort portInt $ setLogger aplogger defaultSettings
    runSettings settings (logStdoutDev (app token))

app :: String -> Application
app token = serve api (server token)

api :: Proxy API
api = Proxy

server :: String -> Server API
server token = handleSlackRequest token :<|> return "Hello World"

handleSlackRequest :: String -> SlackRequest -> Handler SlackResponse
handleSlackRequest token s@SlackMessage {..} =
  let slackUser = slackMessageUser slackEvent
  in  if slackUser /= "unitbot"
        then liftIO (forkIO (sendMessage token s))
          >> return SlackResponseNothing
        else return SlackResponseNothing
handleSlackRequest _ SlackRequest { slackChallenge } =
  return (SlackResponse slackChallenge)

sendMessage :: String -> SlackRequest -> IO ()
sendMessage token SlackMessage {..} = do
  let slackMessage = htmlEncodedText . slackMessageText $ slackEvent
  unless (_isConversion $ convert slackMessage)
    $ case Quantities.fromString (convert slackMessage) of
        Right quantity -> _sendMessage quantity
        _              -> return ()
    -- "=>" should be contained to be considered that this is a conversion text

 where
  _isConversion :: Text -> Bool
  _isConversion s = "=>" `isInfixOf` s
  _sendMessage :: (Eq a, Ord a, Show a) => Quantities.Quantity a -> IO ()
  _sendMessage quantity = do
    let slackChannel  = slackMessageChannel slackEvent
    let slackThreadTs = slackMessageThreadTs slackEvent
    let slackBotMessage = SlackBotMessage slackToken
                                          slackChannel
                                          (pack . show $ quantity)
                                          slackThreadTs
    let opts =
          Wreq.defaults
            &  Wreq.header "Authorization"
            .~ [encodeUtf8 ("Bearer " <> pack token)]
    response <- Wreq.postWith opts
                              "https://slack.com/api/chat.postMessage"
                              (toJSON slackBotMessage)
    TIO.putStrLn $ "Sent a message: " <> convert (show slackBotMessage)
    TIO.putStrLn $ "Received a response: " <> convert
      (show (response ^. Wreq.responseBody))
sendMessage _ _ = return ()
