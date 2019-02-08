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

import Statistics.Distribution.Empirical (levelEmpiricalDistribution, cumulativeEmpiricalDistribution)

import Widget.Title
import Widget.Splits
import Widget.Times
import Widget.Total
import Widget.Percentile

data Layout = L Splits Percentile Times Title Total

instance Widget Layout where
  update = updateLayout
  handle = handleLayout
  redraw = redrawLayout
  window = error "Called `window` on Layout"
  move   = error "Called `move` on Layout"
  resize = error "Called 'resize' on Layout"


mkLayout :: Config -> FileFormat -> Curses Layout
mkLayout (Config title levels) file = do
  -- Window setup
  let levelNames      = map levelName levels
      levelBestTimes  = map best      levels
      cumulativeTimes = map cumu      levels
      levelDistrs     = map snd $ levelEmpiricalDistribution file
      percentileData  = zip levelDistrs levelBestTimes

  layout <- L <$> (mkSplits levelNames          <$> newWindow')
              <*> (mkPercentile percentileData  <$> newWindow')
              <*> (mkTimes file                 <$> newWindow')
              <*> (mkTitle file title           <$> defaultWindow)
              <*> (mkTotal file cumulativeTimes <$> newWindow')

  arrangeLayout layout

  return layout


updateLayout :: NominalDiffTime -> Layout -> Curses Layout
updateLayout d (L s p times title to) =
  let f = update d
  in L <$> f s
       <*> f p
       <*> f times
       <*> f title
       <*> f to


handleLayout :: TimerAction -> Layout -> Curses Layout
handleLayout ta (L s p times title to) =
  let f = handle ta
  in L <$> f s
       <*> f p
       <*> f times
       <*> f title
       <*> f to


redrawLayout :: Layout -> Curses ()
redrawLayout (L s p times title to) = do
  redraw s
  redraw p
  redraw times
  redraw title
  redraw to


newWindow' :: Curses Window
newWindow' = newWindow 1 1 0 0


arrangeLayout :: Layout -> Curses ()
arrangeLayout (L splits percentiles times title total) = do
  (rows, columns) <- screenSize

  let titleRows  = 2
      totalsRows = 4
      splitsRows = rows - (titleRows + totalsRows)

      timesColumns      = 10
      percentileColumns = 5
      splitsColumns     = columns - (timesColumns + percentileColumns)


  move   title 0 0
  resize title titleRows columns
  redraw title

  move   splits titleRows 0
  resize splits splitsRows splitsColumns
  redraw splits

  move   percentiles titleRows splitsColumns
  resize percentiles splitsRows percentileColumns
  redraw percentiles

  move   times titleRows (splitsColumns + percentileColumns)
  resize times splitsRows timesColumns
  redraw times

  move   total (titleRows + splitsRows) 0
  resize total totalsRows columns
  redraw total


resetLayout :: Config -> FileFormat -> Layout -> Layout
resetLayout (Config title levels) file (L splits percentiles times titleW total) =
  let cumulativeTimes = map cumu levels
  in L splits
       percentiles
       (mkTimes file $ window times)
       (mkTitle file title $ window titleW)
       (mkTotal file cumulativeTimes $ window total)
