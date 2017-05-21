module Main where

import Import
import File
import Config

import Widget.Splits
import Widget.Times
import Widget.Total
import Widget.Percentile

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import Data.Yaml


main :: IO ()
main = do
  -- YAML setup
  yml <- liftIO $ decodeFileEither "splits.yaml"

  case yml of
    Right c -> doCurses c
    Left  p -> putStrLn $ prettyPrintParseException p


doCurses :: Config -> IO ()
doCurses (Config levels) = runCurses $ do
  -- NCurses setup
  setEcho False
  setCursorMode CursorInvisible

  -- Time setup
  startTime <- liftIO getCurrentTime

  -- Window setup
  ss <- screenSizes

  let levelNames = map name levels
      levelTimes = map time levels
      cumulativeTimes = map cumu levels

  -- Load previous data
  file <- liftIO $ load levelNames

  splits      <- mkSplits levelNames <$> defaultWindow
  percentiles <- mkPercentile levelTimes <$> newWindow (splitsRows ss) (percentilesColumns ss) 0 (splitsColumns ss)
  times <- mkTimes file <$> newWindow (splitsRows ss) (timesColumns ss) 0 (splitsColumns ss + percentilesColumns ss)
  total <- mkTotal file cumulativeTimes <$> newWindow (totalsRows ss) (columns ss) (splitsRows ss) 0

  resize splits (splitsRows ss) (splitsColumns ss)

  loop file cumulativeTimes splits percentiles times total startTime

  where loop file cumulativeTimes splits percentiles times total startTime = do
          redraw splits
          redraw percentiles
          redraw times
          redraw total

          render

          (newFile, newSplits, newPercentiles, newTimes, newTotal, continue) <-
            mainLoop file splits percentiles times total startTime (Paused True)

          if continue
            then do
              startTime <- liftIO getCurrentTime
              loop newFile cumulativeTimes newSplits newPercentiles (mkTimes newFile $ window newTimes) (mkTotal newFile cumulativeTimes $ window newTotal) startTime
            else
              return ()


mainLoop :: FileFormat -> Splits -> Percentile -> Times -> Total -> UTCTime -> Paused -> Curses (FileFormat, Splits, Percentile, Times, Total, Bool)
mainLoop file splits percentiles times total lastTime p = do
  ev       <- getEvent (window splits) (Just 60)
  thisTime <- liftIO getCurrentTime

  let deltaT = if paused p
                 then toEnum 0
                 else diffUTCTime thisTime lastTime

  (nf, eventSplits, eventPercentiles, eventTimes, eventTotal, p', done, continue) <- handleEvents ev

  if done
    then return (nf, eventSplits, eventPercentiles, eventTimes, eventTotal, continue)
    else do
      newSplits <- update deltaT eventSplits
      newTimes  <- update deltaT eventTimes
      newTotal  <- update deltaT eventTotal

      newPercentile <- update (curs $ timesZipper newTimes) eventPercentiles

      render

      mainLoop nf newSplits newPercentile newTimes newTotal thisTime p'

  where handleEvents Nothing   = return (file, splits, percentiles, times, total, p, False, True)
        handleEvents (Just ev) = do
          action <-
            case ev of
              EventResized -> do
                ss <- screenSizes

                move   splits 0 0
                resize splits (splitsRows ss) (splitsColumns ss)
                redraw splits

                move   percentiles 0 (splitsColumns ss)
                resize percentiles (splitsRows ss) (percentilesColumns ss)
                redraw percentiles

                move   times 0 (splitsColumns ss + percentilesColumns ss)
                resize times (splitsRows ss) (timesColumns ss)
                redraw times

                move   total (splitsRows ss) 0
                resize total (totalsRows ss) (columns ss)
                redraw total

                return Pass

              -- The "pause toggle" key was pressed
              EventCharacter 'p' ->
                if paused p
                  then return Start
                  else return Pause

              -- Alternate "pause toggle" key, corresponding to the standard "enter" or "return" key
              EventCharacter '\n' ->
                if paused p
                  then return Start
                  else return Pause

              -- Advance the split from start to finish
              EventCharacter ' ' ->
                if paused p
                  then return Start
                  else if null $ right $ splitsZipper splits
                         then return End
                         else return Advance

              -- Return to the previous split
              EventSpecialKey KeyPreviousPage ->
                return Reverse

              -- Skip the next split and invalidate the current one
              EventSpecialKey KeyNextPage ->
                if null $ right $ splitsZipper splits
                  then return Pass
                  else return Skip

              -- Saves the current data to a file
              EventCharacter 's' ->
                if paused p
                  then return (Save $ recordRun file (zipperToList $ timesZipper times)
                                                     (zipperToList $ validityZipper times))
                  else return Pass

              -- Reset without saving
              EventCharacter 'r' ->
                if paused p
                  then return Reset
                  else return Pass

              otherwise -> return Pass

          (action', nf) <- 
            case action of
              Save nf   -> liftIO $ save nf
                        >> return (Reset, nf)
              Reset     -> return (Reset, file)
              otherwise -> return (action, file)

          newSplits      <- handle action' splits
          newPercentiles <- handle action' percentiles
          newTimes       <- handle action' times
          newTotal       <- handle action' total

          let done = ev == EventCharacter 'q'
                  || ev == EventCharacter 'Q'
                  || action' == Reset

          let p' = case action' of
                     Start -> Paused False
                     Pause -> Paused True
                     End   -> Paused True
                     _     -> p

          return (nf, newSplits, newPercentiles, newTimes, newTotal, p', done, action' == Reset)


data ScreenSizes = ScreenSizes { rows    :: Integer
                               , columns :: Integer
                               , splitsRows    :: Integer
                               , splitsColumns :: Integer
                               , timesColumns  :: Integer
                               , totalsRows    :: Integer
                               , percentilesColumns :: Integer}

screenSizes = do
  (rows, columns) <- screenSize

  let timesColumns      = 10
      totalsRows        = 4
      percentileColumns = 5
      splitsColumns     = columns - (timesColumns + percentileColumns)
      splitsRows        = rows    - totalsRows

  return $ ScreenSizes rows columns splitsRows splitsColumns timesColumns totalsRows percentileColumns


waitFor :: (Event -> Bool) -> (Event -> Curses ()) -> Curses () -> Curses ()
waitFor p q r = loop where
    loop = do
        w <- defaultWindow
        ev <- getEvent w (Just 100)
        case ev of
            Nothing -> r >> loop
            Just ev' -> if p ev' then return () else q ev' >> r >> loop
