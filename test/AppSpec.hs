{-# LANGUAGE OverloadedStrings #-}
module AppSpec
  ( spec
  ) where

import App (app)
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec =
  with (return $ app "token") $ do
    describe "GET /healthcheck" $ do

      it "responds with 200" $ do
        get "/healthcheck" `shouldRespondWith` 200

      it "responds with 'Hello World'" $ do
        get "/healthcheck" `shouldRespondWith` "Hello World"
