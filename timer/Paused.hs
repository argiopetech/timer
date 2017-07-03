module Paused where

newtype Paused = Paused Bool

pnot (Paused b) = Paused $ not b

paused (Paused b) = b
