{-# LANGUAGE OverloadedStrings #-}

module Config where

import Types

import Data.Text
import Data.Yaml

data Config = Config [Level]

instance FromJSON Config where
  parseJSON (Object o) = Config <$> o .: "levels"

data Level = Level { name :: Text
                   , time :: NormalParams
                   , cumu :: NormalParams
                   }

instance FromJSON Level where
  parseJSON (Object l) = do
    t <- l .: "time"
    c <- l .: "cumulative"

    Level <$> l .: "name"
          <*> ((,) <$> t .: "mean"
                   <*> t .: "sigma")
          <*> ((,) <$> c .: "mean"
                   <*> c .: "sigma")
