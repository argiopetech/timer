{-# LANGUAGE GADTs #-}
module Format.Percentile
  ( Percentile()
  , percentile
  , ignoreHigh
  ) where

import Data.Time.Clock
import Statistics.Distribution
import Statistics.Distribution.Normal
import Text.Printf

data Percentile where
  Percentile :: Distribution d => d -> Double -> Percentile

instance Show Percentile where
  showsPrec _ = showPercentile


percentile :: (Double, Double) -> NominalDiffTime -> Percentile
percentile a b = Percentile (uncurry normalDistr a) $ realToFrac b


showPercentile :: Percentile -> ShowS
showPercentile = showString . go . calc
  where go p | p <  0.1  = replicate 4 '\8595'
             | p > 99.9  = replicate 4 '\8593'
             | otherwise = printf "%4.1f" p

ignoreHigh :: Percentile -> String
ignoreHigh a = go $ calc a
  where go p | p > 99.9  = "   "
             | otherwise = show a


calc :: Percentile -> Double
calc (Percentile d t) =
  let b = complCumulative d t
  in (/10) $ fromIntegral $ (round :: Double -> Integer) $ b * 1000
