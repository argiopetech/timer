module Main where

import Config
import File
import Format.SplitTime

import Statistics.Distribution
import Statistics.Distribution.Empirical

import Control.Monad.IO.Class (liftIO)
import Data.Yaml


main :: IO ()
main = do
  -- YAML setup
  yml <- liftIO $ decodeFileEither "splits.yaml"

  f <- case yml of
         Right (Config _ levels) -> load levels
         Left  _ -> load'

  let distrs = levelEmpiricalDistribution f
      l1Data = snd $ head $ onlyValidSplits $ levelData f
      m      = realToFrac $ minimum l1Data
      ma     = realToFrac $ maximum l1Data
      range  = [m, m+0.001..ma]

  print $ map (cumulative (snd $ head distrs)) range
