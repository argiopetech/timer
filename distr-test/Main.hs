module Main where

import Config
import File

import Statistics.Distribution
import Statistics.Distribution.Empirical

import Data.List (sort)
import Data.Yaml

import Text.Printf


main :: IO ()
main = do
  -- YAML setup
  yml <- decodeFileEither "splits.yaml"

  f <- case yml of
         Right (Config _ levels) -> load levels
         Left  _ -> load'

  let distrs = levelEmpiricalDistribution f
      l1Data = map realToFrac $ sort $ snd $ head $ onlyValidSplits $ levelData f
      m      = minimum l1Data
      ma     = maximum l1Data
      domain = [m, m+0.001..ma]
      range  = map (cumulative (snd $ head distrs)) l1Data -- domain

  mapM_ putStrLn $ map (uncurry $ printf "%f %f") $ zip domain range
