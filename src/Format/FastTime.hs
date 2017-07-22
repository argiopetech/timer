{-# LANGUAGE GADTs #-}
module Format.FastTime
  ( FastTime()
  , fastTime
  , ignoreHigh
  ) where

import Data.Time.Clock
import Statistics.Distribution
import Statistics.Distribution.Normal
import Text.Printf

newtype FastTime where
  FastTime :: Double -> FastTime

instance Show FastTime where
  showsPrec _ = showFastTime


fastTime :: NominalDiffTime -> FastTime
fastTime = FastTime . realToFrac


showFastTime :: FastTime -> ShowS
showFastTime (FastTime p) = showString go
  where go | p <= (-1000) = replicate 4 '\8593'
           | p >=      0  = replicate 4 '\8595'
           | p <=   (-10) = printf "-%3.0f" $ abs p
           | otherwise    = printf "%4.1f" p

ignoreHigh :: FastTime -> String
ignoreHigh a = go $ calc a
  where go p | p <= (-10) = "   "
             | otherwise  = show a


calc :: FastTime -> Double
calc (FastTime t) = undefined
