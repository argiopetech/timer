module Widget.Total
  ( Total()
  , mkTotal
  ) where

import Import
import File
import Format.SplitTime
import Format.Percentile

import Control.Monad (unless)
import Text.Printf

data Total = Total FileFormat (Zipper NormalParams) (Zipper NominalDiffTime) Window

instance Widget Total where
  update                 = updateTotal
  handle                 = handleTotal
  redraw                 = redrawTotal
  window (Total _ _ _ w) = w


mkTotal :: FileFormat -> [NormalParams] -> Window -> Total
mkTotal f p = Total f (listToZipper p) (listToZipper [0])


updateTotal :: NominalDiffTime -> Total -> Curses Total
updateTotal d (Total f c z w) = updateWindow w $ do
  let nz = replace (curs z + d) z
      nt = Total f c nz w
      str = show $ splitTime (sum $ zipperToList nz)

  (rows, columns) <- windowSize

  moveCursor 1 (columns - 14)
  drawString $
    if null $ left z
      then "----"
      else if null $ right c
             then show $ percentile (curs c) (sum $ zipperToList z)
             else show $ percentile (curs $ prev c) (sum $ left z)


  moveCursor 1 (columns - (fromIntegral $ length str))
  insertString str

  return nt


handleTotal :: TimerAction -> Total -> Curses Total
handleTotal Reset   (Total f c _ w) = return $ mkTotal f (zipperToList c) w
handleTotal Advance (Total f c z w) = return $ Total f (next c) (push 0 z) w
handleTotal Reverse (Total f c z w) =
  return $
    if null $ left z
      then Total f (prev c) z w
      else
        let cz = curs z
            z' = trunc $ prev z
            nz = replace (curs z' + cz) z'
        in Total f (prev c) nz w
handleTotal Skip    (Total f c z w) = return $ Total f (next c) (push 0 z) w
handleTotal _ t = return t


redrawTotal :: Total -> Curses ()
redrawTotal t@(Total f c z w) = updateWindow w $ do
  (rows, columns) <- windowSize

  erase

  -- Top horizontal line
  -- This is the only thing in row 0
  moveCursor 0 0
  drawLineH Nothing columns

  -- Current total
  -- Tag
  moveCursor 1 (columns - 22)
  drawString "Total:"

  -- Percentile
  moveCursor 1 (columns - 14)
  insertString $
    if null $ left z
      then "----"
      else if null $ right c
             then show $ percentile (curs c) (sum $ zipperToList z)
             else show $ percentile (curs $ prev c) (sum $ left z)

  -- Current cumulative time over all splits
  do
     let str = show $ splitTime $ sum $ zipperToList z

     moveCursor 1 (columns - (fromIntegral $ length str))
     insertString str


  -- The following are consistent internal to a run
  let target = if null $ right c
                 then curs c
                 else last $ right c

  -- Personal Best
  -- Tag
  moveCursor 2 (columns - 30)
  drawString "Personal Best:"

  do
    let pb  = personalBest f
        str = denoteZero $ splitTime pb
        p   = percentile target pb

    -- Percentile
    moveCursor 2 (columns - 14)
    insertString $ ignoreHigh p

    -- Current personal best
    moveCursor 2 (columns - (fromIntegral $ length str))
    insertString str

  -- Draw Sum of Bests
  -- Tag
  moveCursor 3 (columns - 29)
  drawString "Sum of Bests:"

  do
    let sob = sumOfBests f
        str = denoteZero $ splitTime $ sob
        p   = percentile target sob

    -- Percentile
    moveCursor 3 (columns - 14)
    insertString $ ignoreHigh p

    -- Current sum of bests
    moveCursor 3 (columns - (fromIntegral $ length str))
    insertString str


  -- Various vertical lines
  moveCursor 0 (columns - 15)
  drawGlyph glyphPlus

  moveCursor 1 (columns - 15)
  drawLineV Nothing 3

  moveCursor 0 (columns - 10)
  drawGlyph glyphPlus

  moveCursor 1 (columns - 10)
  drawLineV Nothing 3
