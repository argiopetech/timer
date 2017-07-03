module Main where

import File
import Format.SplitTime

import qualified Data.Text as T
import Text.Printf

main :: IO ()
main = do
  f@(FileFormat a _ c) <- load'
  let lNames = map fst a
      bests  = map snd $ levelBests f
      longestName = maximum $ map T.length lNames

  putStrLn $ unlines $ zipWith (\a b -> printf "%-*s" longestName a ++ " - " ++ show (splitTime b)) lNames bests
