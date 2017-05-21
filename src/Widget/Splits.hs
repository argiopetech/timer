module Widget.Splits
  ( Splits()
  , mkSplits
  , splitsZipper
  ) where

import Import

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)


data Splits = Splits (Zipper Text) Window

instance Widget Splits where
  update              = updateSplits
  handle              = handleSplits
  redraw              = redrawSplits
  window (Splits _ w) = w


mkSplits :: [Text] -> Window -> Splits
mkSplits splitLabels = Splits (listToZipper splitLabels)


splitsZipper :: Splits -> Zipper Text
splitsZipper (Splits z _) = z

updateSplits :: NominalDiffTime -> Splits -> Curses Splits
updateSplits _ s = return s


handleSplits :: TimerAction -> Splits -> Curses Splits
handleSplits Advance (Splits z w) =
  let s = Splits (next z) w
  in redrawSplits s >> return s

handleSplits Reverse (Splits z w) =
  let s = Splits (prev z) w
  in redrawSplits s >> return s

handleSplits Skip (Splits z w) =
  let s = Splits (next z) w
  in redrawSplits s >> return s

handleSplits Reset (Splits z w) = return $ Splits (first z) w

handleSplits _ s = return s


redrawSplits :: Splits -> Curses ()
redrawSplits (Splits z w) = do
  updateWindow w $ do
    (rows, columns) <- windowSize

    erase

    let (l, c, r) = sizeZipperToScreen rows z
        la        = replicate (length l) []
        ca        = [AttributeStandout]
        ra        = replicate (length r) []
        as        = la ++ ca:ra

    mapM_ (\(a, b, c) -> moveCursor a 0 >> setAttributes b >> drawText c) $ zip3 [0..] as $ l ++ c:r
