module Statistics.Distribution.Empirical where

import File

import Data.List     (sort, partition)
import Data.Time.Clock
import Statistics.Distribution (Distribution, cumulative)

import qualified Data.IntMap as IM


newtype EmpiricalDistribution = EmpiricalDistribution (NominalDiffTime -> Double)

instance Distribution EmpiricalDistribution where
  cumulative (EmpiricalDistribution f) = f . realToFrac

cumulativeEmpiricalDistribution :: FileFormat -> [(Int, EmpiricalDistribution)]
cumulativeEmpiricalDistribution (FileFormat ls _ runs) =
  let es = IM.elems runs
      lData = [ map (sum . map fst)
                $ filter (\l -> length l == len)
                $ map ( reverse . dropWhile (not . snd) . reverse
                        . IM.elems . IM.filterWithKey (\k _ -> (k <= a))) es
              | (a, len) <- zip (map snd ls) [1..] ]
      fs = map toF lData
  in zip (map snd ls) fs

levelEmpiricalDistribution :: FileFormat -> [(Int, EmpiricalDistribution)]
levelEmpiricalDistribution ff =
  let (ls, lData) = unzip $ onlyValidSplits $ levelData ff
      fs = map toF lData
  in zip ls fs


toF :: [NominalDiffTime] -> EmpiricalDistribution
toF ls =
  let slData = sort ls
      tLen   = fromIntegral $ length slData
      f = \t -> let (lp, gp) = partition (<= t) slData
                in case (reverse lp, gp) of
                     ([], []) -> 0.0
                     ([], _ ) -> 0.0
                     (_,  []) -> 1.0
                     (y1:y0:_, y2:y3:_) ->
                       let m = mu (y1, y2) t
                           lLen = fromIntegral $ length lp
                           p = \o -> (lLen + o) / tLen
                       in hermiteInterpolate (p (-1), p 0, p 1, p 2) m
                     (l:_,  m:_ ) ->
                       let lLen = fromIntegral $ length lp
                       in linearInterpolate (l, m) (lLen / tLen, (lLen + 1) / tLen) t

  in EmpiricalDistribution f
  where
    linearInterpolate :: (NominalDiffTime, NominalDiffTime) -> (Double, Double) -> NominalDiffTime -> Double
    linearInterpolate domain (lRange, uRange) domainVal =
      let m    = mu domain domainVal
          notM = 1 - m
      in lRange * notM + uRange * m

    mu :: RealFrac a => (a, a) -> a -> Double
    mu (lDomain, uDomain) domainVal = realToFrac (domainVal - lDomain) / realToFrac (uDomain - lDomain)

    hermiteInterpolate :: (Double, Double, Double, Double) -> Double -> Double
    hermiteInterpolate (y0, y1, y2, y3) m =
      let m2 = m * m
          a0 = (-0.5) * y0 + 1.5 * y1 - 1.5 * y2 + 0.5 * y3
          a1 = y0 - 2.5 * y1 + 2 * y2 - 0.5 * y3
          a2 = (-0.5) * y0 + 0.5 * y2
          a3 = y1
      in a0 * m * m2 + a1 * m2 + a2 * m + a3
