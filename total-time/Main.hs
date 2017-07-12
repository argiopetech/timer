module Main where

import File
import Format.SplitTime


main :: IO ()
main = do
  f <- load'

  print $ splitTime $ playTime f
