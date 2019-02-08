module Main where

import Config
import File
import Format.SplitTime

import Statistics.Distribution
import Statistics.Distribution.Empirical

import Control.Monad.IO.Class (liftIO)
import Data.Yaml

import Text.Printf


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
      domain = [m, m+0.001..ma]
      range  = map (cumulative (snd $ head distrs)) domain

  mapM_ putStrLn $ map (uncurry $ printf "%f %f") $ zip domain range
