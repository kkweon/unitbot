{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Slack.Model where

import           Control.Monad                  ( mzero )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , Value(Object)
                                                , (.:)
                                                , (.:?)
                                                , fieldLabelModifier
                                                , parseJSON
                                                , withObject
                                                )
import           Data.Aeson.TH                  ( defaultOptions
                                                , deriveJSON
                                                )
import qualified Data.HashMap.Strict           as HML
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

-- | Request JSON object sent by Slack
data SlackRequest
  = SlackRequest
      { slackToken :: Text
      , slackChallenge :: Text
      , slackType :: Text
      }
  | SlackMessage
      { slackToken :: Text
      , slackTeamId :: Text
      , slackApiAppId :: Text
      , slackEvent :: SlackMessageEvent
      , slackType :: Text
      , slackAuthedTeams :: [Text]
      , slackEventId :: Text
      , slackEventTime :: Int
      }

instance FromJSON SlackRequest where
  parseJSON (Object o) = if HML.member "challenge" o
    then SlackRequest <$> o .: "token" <*> o .: "challenge" <*> o .: "type"
    else
      SlackMessage
      <$> o
      .:  "token"
      <*> o
      .:  "team_id"
      <*> o
      .:  "api_app_id"
      <*> o
      .:  "event"
      <*> o
      .:  "type"
      <*> o
      .:  "authed_users"
      <*> o
      .:  "event_id"
      <*> o
      .:  "event_time"
  parseJSON _ = mzero

-- | Message Event Object sent by Slack
data SlackMessageEvent =
  SlackMessageEvent
    { slackMessageType :: Text
    , slackMessageUser :: Text
    , slackMessageText :: Text
    , slackMessageClientMsgId :: Text
    , slackMessageTs :: Text
    , slackMessageThreadTs :: Maybe Text
    , slackMessageChannel :: Text
    , slackMessageEventTs :: Text
    , slackMessageChannelType :: Text
    }
  deriving (Show, Eq)

instance FromJSON SlackMessageEvent where
  parseJSON = withObject "SlackMessageEvent" $ \v ->
    SlackMessageEvent
      <$> v
      .:  "type"
      <*> v
      .:  "user"
      <*> v
      .:  "text"
      <*> v
      .:  "client_msg_id"
      <*> v
      .:  "ts"
      <*> v
      .:? "thread_ts"
      <*> v
      .:  "channel"
      <*> v
      .:  "event_ts"
      <*> v
      .:  "channel_type"

-- | Response
data SlackResponse
  = SlackResponse
      { challenge :: Text
      } -- ^ Initial hand shaking response
  | SlackResponseNothing -- ^ No need to respond since reply has to be made via another HTTP request
  deriving (Generic, Show)

instance ToJSON SlackResponse

-- | Message sent by Bot
data SlackBotMessage =
  SlackBotMessage
    { slackBot_token :: Text
    , slackBot_channel :: Text
    , slackBot_text :: Text
    , slackBot_thread_ts :: Maybe Text
    }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 9} ''SlackBotMessage)
