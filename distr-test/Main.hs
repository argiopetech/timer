{-# LANGUAGE TypeApplications #-}
module Main where

import Config
import File

import Statistics.Distribution
import Statistics.Distribution.Empirical
import Statistics.Distribution.Normal

import Data.List
import Data.Yaml

import Text.Printf

import Debug.Trace


main :: IO ()
main = do
  -- YAML setup
  yml <- decodeFileEither "splits.yaml"

  f <- case yml of
         Right (Config _ levels) -> load levels
         Left  _ -> load'

  let l = 1
      (Just distrs) = lookup l $ levelEmpiricalDistribution f
      (Just level) = fmap (sort . map realToFrac) . lookup l $ onlyValidSplits $ levelData f
      elems = [0, 0.01..0.99]
      dens = 1 / (fromIntegral $ length level)
      domain = reverse [1.0, 1.0 - dens..dens]

      vec = zipWith Vector2 level domain

      vecs = zip4 vec (drop 1 vec) (drop 2 vec) (drop 3 vec)

      vals = concat $ [ [ let (Vector2 x y) = centripetalInterpolate v m in (x, y) | m <- elems ] | v <- vecs ]

      (xs, ys) = unzip vals

      test = zipWith (\(x1, y1) (x2, y2) -> (abs $ 1 / (y2 - y1)) * (x2 - x1)) vals $ tail vals
      (dx, dy) = unzip $ zipWith (\(x1, y1) (x2, y2) -> (,) (x2 - x1) (y2 - y1)) vals $ tail vals

  mapM_ putStrLn $ zipWith5 (printf "%f %f %f %f %f") xs ys dx dy test


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

centripetalInterpolate (p0, p1, p2, p3) mu =
  let t0 = 0
      t1 = getT t0 p0 p1
      t2 = getT t1 p1 p2
      t3 = getT t2 p2 p3

      t = linearInterpolate (t1, t2) mu

      a1 = vscale ((t1 - t) / (t1 - t0)) p0 + vscale ((t - t0) / (t1 - t0)) p1
      a2 = vscale ((t2 - t) / (t2 - t1)) p1 + vscale ((t - t1) / (t2 - t1)) p2
      a3 = vscale ((t3 - t) / (t3 - t2)) p2 + vscale ((t - t2) / (t3 - t2)) p3

      b1 = vscale ((t2 - t) / (t2 - t0)) a1 + vscale ((t - t0) / (t2 - t0)) a2
      b2 = vscale ((t3 - t) / (t3 - t1)) a2 + vscale ((t - t1) / (t3 - t1)) a3
  in vscale ((t2 - t) / (t2 - t1)) b1 + vscale ((t - t1) / (t2 - t1)) b2
  where getT tn (Vector2 x1 y1) (Vector2 x2 y2) = tn + ((sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2) ** alpha)
        alpha = 0.5


type Scalar = Double

class Vector v where
  vmap  :: (Scalar -> Scalar) -> v -> v
  vzip  :: (Scalar -> Scalar -> Scalar) -> v -> v -> v
  vfold :: (x -> Scalar -> x) -> x -> v -> x

vdot :: Vector v => v -> v -> Scalar
vdot v0 v1 = vfold (+) 0 $ vzip (*) v0 v1

vmag_sqr :: Vector v => v -> Scalar
vmag_sqr v = v `vdot` v

vmag :: Vector v => v -> Scalar
vmag = sqrt . vmag_sqr

vscale :: Vector v => Scalar -> v -> v
vscale s = vmap (s*)

vunit :: Vector v => v -> v
vunit v =
  if vmag v == 0
    then v
    else vscale (1 / vmag v) v


data Vector2 = Vector2 {v2x, v2y :: Scalar} deriving (Eq)

instance Show Vector2 where
  show (Vector2 x y) = "<" ++ (show x) ++ ", " ++ (show y) ++ ">"

instance Vector Vector2 where
  vmap  f   (Vector2 x y) = Vector2 (f x) (f y)
  vfold f i (Vector2 x y) = (i `f` x) `f` y
  vzip  f   (Vector2 x0 y0) (Vector2 x1 y1) = Vector2 (f x0 x1) (f y0 y1)

instance Num Vector2 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  negate = vmap negate
  fromInteger s = Vector2 (fromInteger s) (fromInteger s)

instance Fractional Vector2 where
  (/) = vzip (/)
  fromRational s = let r = realToFrac s in Vector2 r r
