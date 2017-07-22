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
                   , time      :: (Double, Double, NominalDiffTime)
                   , cumu      :: NormalParams
                   } deriving (Show)

instance ToJSON Level where
  toJSON (Level n (tm, tsd, tb) (cm, csd)) = object [ "name"       .= n
                                                    , "time"       .= object ["shape" .= tm, "scale" .= tsd, "best" .= tb]
                                                    , "cumulative" .= object ["mean" .= cm, "sigma" .= csd] ]

instance FromJSON Level where
  parseJSON (Object l) = do
    t <- l .: "time"
    c <- l .: "cumulative"

    Level <$> l .: "name"
          <*> ((,,) <$> t .: "shape"
                    <*> t .: "scale"
                    <*> t .: "best")
          <*> ((,) <$> c .: "mean"
                   <*> c .: "sigma")

prettyConfig = setConfCompare go defConfig
  where go "title"  "levels" = LT
        go "levels" "title"  = GT

        go "name"   "time"       = LT
        go "name"   "cumulative" = LT

        go "time"   "name"       = GT
        go "time"   "cumulative" = LT

        go "cumulative" "name" = GT
        go "cumulative" "time" = GT

        go "mean"  "sigma" = LT
        go "sigma" "mean"  = GT

        go "best"  "scale" = LT
        go "best"  "shape" = LT
        go "best"  _       = GT

        go "shape" "best"  = GT
        go "shape" _       = LT

        go "scale" _       = GT

        go t1 t2 = compare t1 t2
