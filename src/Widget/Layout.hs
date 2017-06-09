{-# LANGUAGE NoMonomorphismRestriction #-}
module Widget.Layout
  ( Layout(..)
  , mkLayout
  , arrangeLayout
  , resetLayout
  ) where

import Import
import File
import Config

import Widget.Splits
import Widget.Times
import Widget.Total
import Widget.Percentile

data Layout = L Splits Percentile Times Total

instance Widget Layout where
  update = updateLayout
  handle = handleLayout
  redraw = redrawLayout
  window = error "Called `window` on Layout"
  move   = error "Called `move` on Layout"
  resize = error "Called 'resize' on Layout"


mkLayout :: Config -> FileFormat -> Curses Layout
mkLayout (Config levels) file = do
  -- Window setup
  let levelNames      = map name levels
      levelTimes      = map time levels
      cumulativeTimes = map cumu levels

  layout <- L <$> (mkSplits levelNames          <$> defaultWindow)
              <*> (mkPercentile levelTimes      <$> newWindow')
              <*> (mkTimes file                 <$> newWindow')
              <*> (mkTotal file cumulativeTimes <$> newWindow')

  arrangeLayout layout

  return layout


updateLayout :: NominalDiffTime -> Layout -> Curses Layout
updateLayout d (L s p ti to) =
  let f = update d
  in L <$> f s
       <*> f p
       <*> f ti
       <*> f to


handleLayout :: TimerAction -> Layout -> Curses Layout
handleLayout ta (L s p ti to) =
  let f = handle ta
  in L <$> f s
       <*> f p
       <*> f ti
       <*> f to


redrawLayout :: Layout -> Curses ()
redrawLayout (L s p ti to) = do
  redraw s
  redraw p
  redraw ti
  redraw to


newWindow' :: Curses Window
newWindow' = newWindow 1 1 0 0


arrangeLayout :: Layout -> Curses ()
arrangeLayout (L splits percentiles times total) = do
  (rows, columns) <- screenSize

  let timesColumns      = 10
      totalsRows        = 4
      percentileColumns = 5
      splitsColumns     = columns - (timesColumns + percentileColumns)
      splitsRows        = rows    - totalsRows


  move   splits 0 0
  resize splits splitsRows splitsColumns
  redraw splits

  move   percentiles 0 splitsColumns
  resize percentiles splitsRows percentileColumns
  redraw percentiles

  move   times 0 (splitsColumns + percentileColumns)
  resize times splitsRows timesColumns
  redraw times

  move   total splitsRows 0
  resize total totalsRows columns
  redraw total


resetLayout :: Config -> FileFormat -> Layout -> Layout
resetLayout (Config levels) file (L splits percentiles times total) =
  let cumulativeTimes = map cumu levels
  in L splits
       percentiles
       (mkTimes file $ window times)
       (mkTotal file cumulativeTimes $ window total)
