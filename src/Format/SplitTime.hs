module Format.SplitTime
  ( SplitTime()
  , splitTime
  , denoteZero
  , toMilliseconds
  ) where

import Data.Time.Clock
import Text.Printf

data SplitTime = SplitTime { hours :: Int
                           , minutes :: Int
                           , seconds :: Int
                           , milliseconds :: Int }

instance Show SplitTime where
  showsPrec _ (SplitTime h m s ms)
    | h == 0 = showString $ printf "%2d:%02d.%03d" m s ms
    | h < 10 = showString $ printf "%d:%02d:%02d.%1d" h m s (ms `div` 100)
    | otherwise = showString $ printf "%3d:%02d:%02d" h m s


denoteZero :: SplitTime -> String
denoteZero (SplitTime 0 0 0 0) = " -:--.---"
denoteZero s                   = show s

toMilliseconds :: NominalDiffTime -> Int
toMilliseconds t = fromEnum $ t / 1e9


splitTime :: NominalDiffTime -> SplitTime
splitTime diffT =
  let diffTime = toMilliseconds diffT
      ms       = diffTime `mod` 1000
      ts       = diffTime `div` 1000
      h        = ts `div` 3600
      m        = (ts `mod` 3600) `div` 60
      s        = ts `mod` 60
  in SplitTime h m s ms
