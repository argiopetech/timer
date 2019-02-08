{-# LANGUAGE OverloadedStrings #-}
module Config where

import Types

import Data.Text
import Data.Time.Clock
import Data.Yaml
import Data.Yaml.Pretty hiding (Config)


data Config = Config Text [Level]
            deriving (Show)

instance FromJSON Config where
  parseJSON (Object o) = Config <$> o .: "title" <*> o .: "levels"

data Level = Level { levelName :: Text
                   , best      :: NominalDiffTime
                   } deriving (Show)

instance FromJSON Level where
  parseJSON (Object l) = do
    t <- l .: "time"

    Level <$> l .: "name"
          <*> t .: "best"
