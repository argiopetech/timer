{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Text
import Data.Yaml


data Config = Config Text [Text]
            deriving (Show)

instance FromJSON Config where
  parseJSON (Object o) = Config <$> o .: "title" <*> o .: "levels"
