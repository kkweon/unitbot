{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Text (Text)
import qualified Data.Text as T

isTargetMessage :: Text -> Bool
isTargetMessage text =
  let
    (_, eos) = T.breakOn "=>" text
  in
    not $ T.null eos
