module Statistics.Distribution.Empirical where

import File

import Data.List     (sort, partition)
import Data.Time.Clock
import Statistics.Distribution (Distribution, cumulative)


newtype EmpiricalDistribution = EmpiricalDistribution (NominalDiffTime -> Double)

instance Distribution EmpiricalDistribution where
  cumulative (EmpiricalDistribution f) = f . realToFrac

dataToPercentile :: FileFormat -> [(Int, EmpiricalDistribution)]
dataToPercentile f =
  let (ls, lData) = unzip $ onlyValidSplits $ levelData f
      fs = map toF lData
  in zip ls fs
  where toF :: [NominalDiffTime] -> EmpiricalDistribution
        toF ls =
          let slData = sort ls
              tLen   = fromIntegral $ length slData
              f = \t -> let p = partition (<= t) slData
                        in case p of
                             ([], []) -> 0.0
                             ([], _ ) -> 0.0
                             (_,  []) -> 1.0
                             (l,  m ) ->
                               let lBound = last l
                                   uBound = head m
                                   tFrac  = realToFrac (t - lBound) / realToFrac (uBound - lBound)
                                   lLen   = fromIntegral $ length l
                                   lPctl  = lLen / tLen
                                   uPctl  = (lLen + 1) / tLen
                                   delta  = uPctl - lPctl
                               in lPctl + tFrac * delta
          in EmpiricalDistribution f
