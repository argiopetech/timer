module Main where

import Import
import File
import Config

import Widget.Layout
import Widget.Splits
import Widget.Times

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Yaml


main :: IO ()
main = do
  -- YAML setup
  yml <- liftIO $ decodeFileEither "splits.yaml"

  case yml of
    Right c -> doCurses c
    Left  p -> putStrLn $ prettyPrintParseException p


doCurses :: Config -> IO ()
doCurses config@(Config _ levels) = runCurses $ do
  -- NCurses setup
  setEcho False
  _ <- setCursorMode CursorInvisible

  -- Time setup
  startTime <- liftIO getCurrentTime

  -- Window setup
  let levelNames = map levelName levels

  -- Load previous data
  file <- liftIO $ load levelNames

  -- Build the layout (which also "instantiates" all the widgets)
  layout <- mkLayout config file

  -- Start the save/reset wrapper around the main loop
  loop file layout startTime

  where loop file l startTime = do
          redraw l

          render

          (newFile, l', continue) <- mainLoop file l startTime (Paused True)

          when continue $
            liftIO getCurrentTime >>= loop newFile (resetLayout config newFile l')


mainLoop :: FileFormat -> Layout -> UTCTime -> Paused -> Curses (FileFormat, Layout, Bool)
mainLoop file l@(L splits _ times _ _) lastTime p = do
  w        <- defaultWindow
  ev       <- getEvent w (Just 60)
  thisTime <- liftIO getCurrentTime

  let deltaT = if paused p
                 then toEnum 0
                 else diffUTCTime thisTime lastTime

  (nf, eventL, p', done, continue) <- handleEvents ev

  if done
    then return (nf, eventL, continue)
    else do
      l' <- update deltaT eventL

      render

      mainLoop nf l' thisTime p'

  where handleEvents Nothing   = return (file, l, p, False, True)
        handleEvents (Just ev) = do
          action <-
            case ev of
              EventResized -> do
                arrangeLayout l

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
              EventCharacter ' ' | paused p                           -> return Start
                                 | null $ right $ splitsZipper splits -> return End
                                 | otherwise                          -> return Advance

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

              EventCharacter 'i' ->
                return ValidInvalid

              _ -> return Pass

          (action', nf) <-
            case action of
              Save nf   -> liftIO $ save nf
                        >> return (Reset, nf)
              Reset     -> return (Reset, file)
              _         -> return (action, file)

          newLayout <- handle action' l

          let done = ev == EventCharacter 'q'
                  || ev == EventCharacter 'Q'
                  || action' == Reset

          let p' = case action' of
                     Start -> Paused False
                     Pause -> Paused True
                     End   -> Paused True
                     _     -> p

          return (nf, newLayout, p', done, action' == Reset)


waitFor :: (Event -> Bool) -> (Event -> Curses ()) -> Curses () -> Curses ()
waitFor p q r = loop where
    loop = do
        w <- defaultWindow
        ev <- getEvent w (Just 100)
        case ev of
            Nothing  -> r >> loop
            Just ev' -> unless (p ev') $ q ev' >> r >> loop
