{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack.ModelSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Hspec.Wai.JSON            ( json )

import           Data.Aeson                     ( decode )

import           Slack.Model

spec :: Spec
spec = do
  describe "SlackMessageEvent" $ do
    it "should parse successfully with optional fields"
      $ let _json = [json|{
    type: "type",
    user: "user",
    text: "text",
    client_msg_id: "client_msg_id",
    ts: "ts",
    thread_ts: "thread_ts",
    channel: "channel",
    event_ts: "event_ts",
    channel_type: "channel_type"
    }|]
        in  decode _json `shouldBe` Just SlackMessageEvent
              { slackMessageType        = "type"
              , slackMessageUser        = "user"
              , slackMessageText        = "text"
              , slackMessageClientMsgId = "client_msg_id"
              , slackMessageTs          = "ts"
              , slackMessageThreadTs    = Just "thread_ts"
              , slackMessageChannel     = "channel"
              , slackMessageEventTs     = "event_ts"
              , slackMessageChannelType = "channel_type"
              }
    it "should parse successfully without optional fields"
      $ let _json = [json|{
    type: "type",
    user: "user",
    text: "text",
    client_msg_id: "client_msg_id",
    ts: "ts",
    channel: "channel",
    event_ts: "event_ts",
    channel_type: "channel_type"
    }|]
        in  decode _json `shouldBe` Just SlackMessageEvent
              { slackMessageType        = "type"
              , slackMessageUser        = "user"
              , slackMessageText        = "text"
              , slackMessageClientMsgId = "client_msg_id"
              , slackMessageTs          = "ts"
              , slackMessageThreadTs    = Nothing
              , slackMessageChannel     = "channel"
              , slackMessageEventTs     = "event_ts"
              , slackMessageChannelType = "channel_type"
              }
