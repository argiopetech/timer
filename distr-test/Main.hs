module Main where

import Config
import File

import Statistics.Distribution
import Statistics.Distribution.Empirical
import Statistics.Distribution.Normal

import Data.List (sort, zipWith4)
import Data.Yaml

import Text.Printf


main :: IO ()
main = do
  -- YAML setup
  yml <- decodeFileEither "splits.yaml"

  f <- case yml of
         Right (Config _ levels) -> load levels
         Left  _ -> load'

{-
  let l = 2
      (Just distrs) = lookup l $ levelEmpiricalDistribution f
      (Just level) =  lookup l $ onlyValidSplits $ levelData f
      l1Data = map realToFrac $ sort level
      m      = minimum l1Data
      ma     = maximum l1Data
      domain = concat $ zipWith (\a b -> let delta = (b - a) / 1 in init [a, a + delta..b]) l1Data $ tail l1Data
      cdf    = zipWith (\a b -> (b - a) / (ma - m)) domain $ tail domain
      iCdf   = concat $ zipWith (\a b -> let delta = (b - a) / 100 in init [a, a + delta..b]) cdf $ tail cdf
      iDomain = concat $ zipWith (\a b -> let delta = (b - a) / 100 in init [a, a + delta..b]) l1Data $ tail l1Data
      range  = map (cumulative distrs) iDomain
      test = zipWith (*) iCdf iDomain


  let iDomain = [-3.0,-2.9..3.0]
      range   = map (cumulative standard) iDomain
      dr      = zip iDomain range
      test    = zipWith (\(d1, r1) (d2, r2) -> (abs $ 1 / (d2 - d1)) * (r2 - r1)) dr $ tail dr
-}


  let l = 2
      (Just distrs) = lookup l $ levelEmpiricalDistribution f
      (Just level) =  lookup l $ onlyValidSplits $ levelData f
      l1Data = map realToFrac $ sort level
      iDomain = concat $ zipWith4 (\a b c d -> [ hermiteInterpolate (a, b, c, d) e | e <- [0, 0.1..1.0]]) l1Data (tail l1Data) (drop 2 l1Data) (drop 3 l1Data)
      range  = map (cumulative distrs) iDomain
      dr     = zip iDomain range
      test   = zipWith (\(d1, r1) (d2, r2) -> (abs $ 1 / (d2 - d1)) * (r2 - r1)) dr $ tail dr


  mapM_ putStrLn $ zipWith3 (printf "%f %f %f") iDomain range test
--  mapM_ putStrLn $ zipWith (printf "%f %f") iDomain range

linearInterpolate :: (Double, Double) -> Double -> Double
linearInterpolate (lRange, uRange) m =
  let notM = 1 - m
  in lRange * notM + uRange * m

hermiteInterpolate :: (Double, Double, Double, Double) -> Double -> Double
hermiteInterpolate (y0, y1, y2, y3) mu =
  let mu2 = mu * mu
      a0 = (-0.5) * y0 + 1.5 * y1 - 1.5 * y2 + 0.5 * y3
      a1 = y0 - 2.5 * y1 + 2 * y2 - 0.5 * y3
      a2 = (-0.5) * y0 + 0.5 * y2
      a3 = y1
  in a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3
