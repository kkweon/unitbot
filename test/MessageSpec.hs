{-# LANGUAGE OverloadedStrings #-}

module MessageSpec
  ( spec
  ) where

import Message
import Test.Hspec

spec :: Spec
spec = do
  describe "isTargetMessage" $ do
    it "should return True if it contains =>" $
      isTargetMessage "5 liter => oz" `shouldBe` True
    it "should return False if it doesn't contain => " $
      isTargetMessage
        "Lorem doloribus eligendi amet quibusdam quasi. Adipisci adipisicing reiciendis atque ipsum accusantium dolores At eos sint consectetur voluptatibus fuga. Aperiam illo pariatur nostrum cupiditate veritatis Voluptatem quos assumenda natus a" `shouldBe`
      False
