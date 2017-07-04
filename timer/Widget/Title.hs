module Widget.Title
  ( Title()
  , mkTitle
  ) where

import Import
import File

import qualified Data.IntMap as IM
import Data.Text
import Control.Monad (unless)
import Text.Printf

data Title = Title Text Int Window

instance Widget Title where
  update               = updateTitle
  handle               = handleTitle
  redraw               = redrawTitle
  window (Title _ _ w) = w


mkTitle :: FileFormat -> Text -> Window -> Title
mkTitle (FileFormat _ _ rs) title = Title title (IM.size rs)


updateTitle :: NominalDiffTime -> Title -> Curses Title
updateTitle _ t = return t


handleTitle :: TimerAction -> Title -> Curses Title
handleTitle _ t = return t


redrawTitle :: Title -> Curses ()
redrawTitle (Title title runs w) = updateWindow w $ do
  (rows, columns) <- windowSize

  erase

  -- The title and the number of runs
  moveCursor 0 0
  drawText title

  moveCursor 0 (columns - 14)
  drawString $ printf "%s% 9d" "Runs:" runs

  -- Bottom horizontal line
  -- This is the only thing in row 1
  moveCursor 1 0
  drawLineH Nothing columns

  moveCursor 0 (columns - 15)
  drawLineV Nothing 1

  -- Various other glyphs
  -- These will break if the layout is updated. This may be worthy of change in future
  moveCursor 1 (columns - 10)
  drawGlyph glyphTeeT
  
  moveCursor 1 (columns - 15)
  drawGlyph glyphPlus

