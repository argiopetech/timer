module Widget.Percentile
  ( Percentile()
  , mkPercentile
  ) where

import Import
import Format.SplitTime
import qualified Format.Percentile as P

import Control.Monad (when)
import Text.Printf

data Percentile = Percentile (Zipper NormalParams) (Zipper NominalDiffTime) (Zipper Bool) Window

instance Widget Percentile where
  update                      = updatePercentile
  handle                      = handlePercentile
  redraw                      = redrawPercentile
  window (Percentile _ _ _ w) = w


mkPercentile :: [NormalParams] -> Window -> Percentile
mkPercentile p = Percentile (listToZipper p) (listToZipper [toEnum 0]) (listToZipper [True])


updatePercentile :: NominalDiffTime -> Percentile -> Curses Percentile
updatePercentile d (Percentile z ts v w) =
  let ts' = replace (curs ts + d) ts
      p   = Percentile z ts' v w
  in redrawPercentile p >> return p


handlePercentile :: TimerAction -> Percentile -> Curses Percentile
handlePercentile Advance (Percentile z ts v w) =
  return $ Percentile (next z) (push 0 ts) (push True v) w

handlePercentile Reverse p@(Percentile z ts v w) =
  return $
    if null $ left z
      then p
      else
        let c   = curs ts
            ts' = trunc $ prev ts
            v'  = if (not $ curs $ prev $ prev v) && (not $ null $ left $ prev v)
                    then trunc $ prev v
                    else replace True $ trunc $ prev v
        in Percentile (prev z) (replace (curs ts' + c) ts') v' w

handlePercentile Skip    (Percentile z ts v w) =
  return $ Percentile (next z) (push (toEnum 0) ts) (push False $ replace False v) w

handlePercentile Reset   (Percentile z ts v w) = return $ mkPercentile (zipperToList z) w
handlePercentile _ t = return t

redrawPercentile :: Percentile -> Curses ()
redrawPercentile p@(Percentile z t v w) = do
  cid <- newColorID ColorBlack ColorDefault 1

  updateWindow w $ do
    (rows, columns) <- windowSize

    erase

    moveCursor 0 0
    drawLineV Nothing rows

    let (lt, ct, rt) = sizeZipperToScreen rows t
        (lv, cv, rv) = sizeZipperToScreen rows v
        (lz, cz, rz) = sizeZipperToScreen rows z

    mapM_ (go cid)
      $ zip3 [0..] (lv ++ cv:rv)
          $ zipWith P.percentile (lz ++ cz:rz) (lt ++ ct:rt)

  where go cid (a, valid, p) = do
          when valid $ do
            moveCursor a 1
            drawString $ show p
