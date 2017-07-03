module Util.CursesHelpers
  ( sizeZipperToScreen
  ) where

import Util.Zipper

import UI.NCurses

sizeZipperToScreen :: Integral r => r -> Zipper a -> ([a], a, [a])
sizeZipperToScreen screenSize z =
  let optimalCenterPosition = fromEnum $ screenSize `div` 2
      ls                    = reverse $ take optimalCenterPosition $ left z
      needed                = fromEnum screenSize - (length ls + 1)
      rs                    = take needed $ right z
  in  (ls, curs z, rs)
