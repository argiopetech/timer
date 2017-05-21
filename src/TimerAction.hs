module TimerAction where

import File (FileFormat)

data TimerAction = Start
                 | Pause
                 | End
                 | Advance
                 | Reverse
                 | Skip
                 | Reset
                 | Save FileFormat
                 | Pass

instance Eq TimerAction where
  (==) Start    Start    = True
  (==) Pause    Pause    = True
  (==) End      End      = True
  (==) Advance  Advance  = True
  (==) Reverse  Reverse  = True
  (==) Skip     Skip     = True
  (==) Reset    Reset    = True
  (==) (Save _) (Save _) = True
  (==) Pass     Pass     = True
  (==) _        _        = False
