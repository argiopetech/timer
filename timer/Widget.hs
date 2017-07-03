module Widget where

import TimerAction

import Data.Time.Clock (NominalDiffTime)
import UI.NCurses

class Widget w where
  update :: NominalDiffTime -> w -> Curses w
  handle :: TimerAction -> w -> Curses w
  redraw :: w -> Curses ()
  window :: w -> Window

  resize :: w -> Integer -> Integer -> Curses ()
  resize w rows columns =
    updateWindow (window w) $ resizeWindow rows columns

  move   :: w -> Integer -> Integer -> Curses ()
  move w x y =
    updateWindow (window w) $ moveWindow x y
