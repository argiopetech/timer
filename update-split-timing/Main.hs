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
      let nv = V.dropWhile (not . T.isPrefixOf "theta_pred[" . name) v
          (vIndivs, vCumuls) = V.partition (T.isPrefixOf "theta_pred[" . name) $ V.init nv
          indivs = V.toList $ V.map (\l -> (mean l, stdDev l)) vIndivs
          cumuls = V.toList $ V.map (\l -> (mean l, stdDev l)) vCumuls

          hierarchicalPrior = (\l -> (mean l, stdDev l)) $ V.last nv
      
          levelNames = map levelName ls
          nConfig = Config title $ zipWith3 Level levelNames indivs cumuls

      putStrLn $ unlines [ "mu_prior <- " ++ (show $ fst hierarchicalPrior)
                         , "tau_prior <- " ++ (show $ snd hierarchicalPrior) ]
      B.writeFile "splits.yaml" $ encodePretty prettyConfig nConfig
