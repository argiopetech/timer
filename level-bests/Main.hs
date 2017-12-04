module Main where

import Config
import File
import Format.SplitTime

import Control.Monad.IO.Class (liftIO)
import Data.Yaml
import qualified Data.Text as T
import Text.Printf


main :: IO ()
main = do
  -- YAML setup
  yml <- liftIO $ decodeFileEither "splits.yaml"

  f@(FileFormat a _ c) <- case yml of
                            Right (Config _ levels) -> load $ map levelName levels
                            Left  _ -> load'

  let lNames = map fst a
      bests  = map (\a -> if a == toEnum maxBound then 0 else a) $ map snd $ levelBests f
      longestName = maximum $ map T.length lNames

  putStr $ unlines $ zipWith (\a b -> printf "%-*s" longestName a ++ " - " ++ (denoteZero $ splitTime b)) lNames bests
