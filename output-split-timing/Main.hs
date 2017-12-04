module Main where

import Config
import File

import Control.Monad.IO.Class (liftIO)
import Data.Yaml


main :: IO ()
main = do
  -- YAML setup
  yml <- liftIO $ decodeFileEither "splits.yaml"

  f <- case yml of
         Right (Config _ levels) -> load $ map levelName levels
         Left  _ -> error "Can't do this without a YAML"

  putStrLn $ output f
