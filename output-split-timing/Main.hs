module Main where

import File

main :: IO ()
main = output <$> load' >>= putStr
