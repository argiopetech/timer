{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config

import Data.Csv  hiding (encode)
import Data.Yaml hiding (decode)
import Data.Yaml.Pretty hiding (Config)

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import qualified Data.Vector          as V

import Debug.Trace


data Line = Line { name :: !T.Text
                 , mean :: !Double
                 , stdDev :: !Double
                 } deriving (Show)

instance FromRecord Line where
    parseRecord v
        | length v >= 4 = Line <$> v .! 0 <*> v .! 1 <*> v .! 3
        | otherwise     = mempty

main :: IO ()
main = do
  -- YAML setup
  yml <- decodeEither <$> B.readFile "splits.yaml"
  csv <- decode HasHeader <$> BS.readFile "summary.csv"

  case (,) <$> yml <*> csv of
    Left  s -> print s
    Right (Config title ls, v) -> do
      let nv = V.dropWhile (not . T.isPrefixOf "shape[" . name) v
          (vShapes, nv') = V.partition (T.isPrefixOf "shape[" . name) nv
          (vScales, nv'') = V.partition (T.isPrefixOf "scale[" . name) nv'
          (vCumuls, vMins) = V.partition (T.isPrefixOf "cumulative[" . name) nv''

          indivs = V.toList $ V.zipWith3 (\shape scale best -> (mean shape, mean scale, realToFrac $ mean best)) vShapes vScales vMins
          cumuls = V.toList $ V.map (\l -> (mean l, stdDev l)) vCumuls

          levelNames = map levelName ls
          nConfig = Config title $ zipWith3 Level levelNames indivs cumuls

      B.writeFile "splits.yaml" $ encodePretty prettyConfig nConfig
