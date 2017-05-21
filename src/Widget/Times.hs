module Widget.Times
  ( Times()
  , mkTimes
  , timesZipper
  , validityZipper
  ) where

import Import
import File
import Format.SplitTime

import Control.Monad (unless, when)
import Data.List (zip4)

data Times = Times FileFormat Bool (Zipper NominalDiffTime) (Zipper Bool) Window

instance Widget Times where
  update                   = updateTimes
  handle                   = handleTimes
  redraw                   = redrawTimes
  window (Times _ _ _ _ w) = w


mkTimes :: FileFormat -> Window -> Times
mkTimes f = Times f False (listToZipper [toEnum 0]) (listToZipper [True])


timesZipper :: Times -> Zipper NominalDiffTime
timesZipper (Times _ _ z _ _) = z


validityZipper :: Times -> Zipper Bool
validityZipper (Times _ _ _ v _) = v


updateTimes :: NominalDiffTime -> Times -> Curses Times
updateTimes d (Times f p z v w) =
  let z' = replace (curs z + d) z
      nt = Times f p z' v w
  in redrawTimes nt >> return nt


handleTimes :: TimerAction -> Times -> Curses Times
handleTimes Advance (Times f p z v w) =
  let t = Times f p (push 0 z) (push True v) w
  in return t

handleTimes Reverse t@(Times f p z v w) =
  if null $ left z
    then return t
    else
      let c = curs z
          z' = trunc $ prev z
          v' = if (not $ curs $ prev $ prev v) && (not $ null $ left $ prev v)
                 then trunc $ prev v
                 else replace True $ trunc $ prev v
          t = Times f p (replace (curs z' + c) z') v' w
      in return t

handleTimes Skip  (Times f p z v w) = return $ Times f p (push 0 z) (push False $ replace False v) w

handleTimes Reset (Times f _ _ _ w) = return $ mkTimes f w

handleTimes End   (Times f _ z v w) = return $ Times f True z v w

handleTimes Start (Times f _ z v w) = return $ Times f False z v w

handleTimes _ t = return t

redrawTimes :: Times -> Curses ()
redrawTimes (Times f end z v w) = do
  cid <- newColorID ColorBlack ColorDefault 1
  pbc <- newColorID ColorYellow ColorDefault 2

  updateWindow w $ do
    (rows, columns) <- windowSize

    erase

    let (lz, cz, rz) = sizeZipperToScreen rows z
        (lv, cv, rv) = sizeZipperToScreen rows v
        lBests       = map snd $ levelBests f
        lzs          = lz ++ cz:rz
        lvs          = lv ++ cv:rv
        pbs          = if end
                         then (zipWith (<) lzs lBests) ++ repeat False
                         else (zipWith (<) lz  lBests) ++ repeat False

    mapM_ (go cid pbc) $ zip4 [0..] lvs pbs lzs

    clearToBottom

    moveCursor 0 0
    drawLineV Nothing rows

  where go cid pbc (a, valid, pb, b) = do
          moveCursor a 1

          when   pb    $ setColor pbc >> setAttributes [AttributeBold]
          unless valid $ setColor cid >> setAttributes [AttributeBold]
          drawString $ show $ splitTime b

          setColor defaultColorID
          setAttributes []
