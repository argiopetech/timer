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

data Times = Times FileFormat Bool (Zipper NominalDiffTime) (Zipper NominalDiffTime) (Zipper Bool) Window

instance Widget Times where
  update                     = updateTimes
  handle                     = handleTimes
  redraw                     = redrawTimes
  window (Times _ _ _ _ _ w) = w


mkTimes :: FileFormat -> Window -> Times
mkTimes f = Times f False (listToZipper [toEnum 0]) (listToZipper $ map snd $ levelBests f) (listToZipper [True])


timesZipper :: Times -> Zipper NominalDiffTime
timesZipper (Times _ _ z _ _ _) = z


bestsZipper :: Times -> Zipper NominalDiffTime
bestsZipper (Times _ _ _ b _ _) = b


validityZipper :: Times -> Zipper Bool
validityZipper (Times _ _ _ _ v _) = v


updateTimes :: NominalDiffTime -> Times -> Curses Times
updateTimes d (Times f p z b v w) =
  let z' = replace (curs z + d) z
      nt = Times f p z' b v w
  in redrawTimes nt >> return nt


handleTimes :: TimerAction -> Times -> Curses Times
handleTimes Advance (Times f p z b v w) =
  let t = Times f p (push 0 z) (next b) (push True v) w
  in return t

handleTimes Reverse t@(Times f p z b v w) =
  return $
    if null $ left z
      then t
      else
        let c = curs z
            z' = trunc $ prev z
            v' = if not ((curs $ prev $ prev v) || (null $ left $ prev v))
                   then trunc $ prev v
                   else replace True $ trunc $ prev v
            t = Times f p (replace (curs z' + c) z') (prev b) v' w
        in t

handleTimes Skip  (Times f p z b v w) = return $ Times f p (push 0 z) (next b) (push False $ replace False v) w

handleTimes Reset (Times f _ _ _ _ w) = return $ mkTimes f w

handleTimes End   (Times f _ z b v w) = return $ Times f True z b v w

handleTimes Start (Times f _ z b v w) = return $ Times f False z b v w

handleTimes ValidInvalid (Times f p z b v w) = return $ Times f p z b (replace (not $ curs v) v) w

handleTimes _ t = return t

redrawTimes :: Times -> Curses ()
redrawTimes (Times f end z b v w) = do
  cid <- newColorID ColorBlack ColorDefault 1
  pbc <- newColorID ColorYellow ColorDefault 2

  updateWindow w $ do
    (rows, columns) <- windowSize

    erase

    let (lz, cz, rz) = sizeZipperToScreen rows z
        (lb, cb, rb) = sizeZipperToScreen rows b
        (lv, cv, rv) = sizeZipperToScreen rows v
        lzs          = lz ++ cz:rz
        lbs          = lb ++ cb:rb
        lvs          = lv ++ cv:rv
        pbs          = if end
                         then (zipWith (<) lzs lbs) ++ repeat False
                         else (zipWith (<) lz  lb) ++ repeat False

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
