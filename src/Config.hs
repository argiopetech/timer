{-# LANGUAGE OverloadedStrings #-}
module Config where

import Types

import Data.Text
import Data.Time.Clock
import Data.Yaml
import Data.Yaml.Pretty hiding (Config)


data Config = Config Text [Level]
            deriving (Show)

instance ToJSON Config where
  toJSON (Config t ls) = object [ "title"  .= t
                                , "levels" .= ls ]

instance FromJSON Config where
  parseJSON (Object o) = Config <$> o .: "title" <*> o .: "levels"

data Level = Level { levelName :: Text
                   , best      :: NominalDiffTime
                   } deriving (Show)

instance ToJSON Level where
  toJSON (Level n tb) = object [ "name"       .= n
                               , "time"       .= object ["best" .= tb]]

instance FromJSON Level where
  parseJSON (Object l) = do
    t <- l .: "time"

    Level <$> l .: "name"
          <*> t .: "best"

prettyConfig = setConfCompare go defConfig
  where go "title"  "levels" = LT
        go "levels" "title"  = GT

        go "name"   "time"       = LT
        go "name"   "cumulative" = LT

        go "time"   "name"       = GT

        go t1 t2 = compare t1 t2
