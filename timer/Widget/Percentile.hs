module Widget.Percentile
  ( Percentile()
  , mkPercentile
  ) where

import Import
import Format.SplitTime
import qualified Format.Percentile as P
import qualified Format.FastTime   as D

import Control.Monad (when)
import Statistics.Distribution.Gamma
import Text.Printf

data Percentile = Percentile (Zipper NormalParams) (Zipper NominalDiffTime) (Zipper NominalDiffTime) (Zipper Bool) Window

instance Widget Percentile where
  update                        = updatePercentile
  handle                        = handlePercentile
  redraw                        = redrawPercentile
  window (Percentile _ _ _ _ w) = w


mkPercentile :: [(Double, Double, NominalDiffTime)] -> Window -> Percentile
mkPercentile p = Percentile (listToZipper $ map (\(a, b, _) -> (a, b)) p)
                            (listToZipper $ map (\(_, _, c) -> c) p)
                            (listToZipper [toEnum 0])
                            (listToZipper [True])


updatePercentile :: NominalDiffTime -> Percentile -> Curses Percentile
updatePercentile deltaT (Percentile z bs ts v w) =
  let ts' = replace (curs ts + deltaT) ts
      p   = Percentile z bs ts' v w
  in redrawPercentile p >> return p


handlePercentile :: TimerAction -> Percentile -> Curses Percentile
handlePercentile Advance (Percentile z bs ts v w) =
  return $ Percentile (next z) (next bs) (push 0 ts) (push True v) w

handlePercentile Reverse p@(Percentile z bs ts v w) =
  return $
    if null $ left z
      then p
      else
        let c   = curs ts
            ts' = trunc $ prev ts
            v'  = if (not $ curs $ prev $ prev v) && (not $ null $ left $ prev v)
                    then trunc $ prev v
                   else replace True $ trunc $ prev v
        in Percentile (prev z) (prev bs) (replace (curs ts' + c) ts') v' w

handlePercentile Skip    (Percentile z bs ts v w) =
  return $ Percentile (next z) (next bs) (push (toEnum 0) ts) (push False $ replace False v) w

handlePercentile Reset   (Percentile z bs ts v w) =
  return $ mkPercentile (zipWith (\(a, b) c -> (a, b, c)) (zipperToList z) (zipperToList bs)) w

handlePercentile ValidInvalid (Percentile z bs ts v w) =
  return $ Percentile z bs ts (replace (not $ curs v) v) w

handlePercentile _ t = return t

redrawPercentile :: Percentile -> Curses ()
redrawPercentile p@(Percentile z bs t v w) = do
  cid <- newColorID ColorBlack ColorDefault 1

  updateWindow w $ do
    (rows, columns) <- windowSize

    erase

    moveCursor 0 0
    drawLineV Nothing rows

    let (lt, ct, rt) = sizeZipperToScreen rows t
        (lb, cb, rb) = sizeZipperToScreen rows bs
        (lv, cv, rv) = sizeZipperToScreen rows v
        (lz, cz, rz) = sizeZipperToScreen rows z

        t'  = (lt ++ ct:rt)
        bs' = (lb ++ cb:rb)
        v'  = (lv ++ cv:rv)
        z'  = (lz ++ cz:rz)

        distrs = map (uncurry gammaDistr) z'

        scaleTime best current = (current / best) - 1

        percentileF distr best current =
          if current == 0
            then "----"
            else if current >= best && best > 0
                   then show $ P.percentile distr $ scaleTime best current
                   else show $ D.fastTime $ current - best
        percentileFs = zipWith3 percentileF distrs bs' t'

    mapM_ (go cid)
      $ zip3 [0..] v' percentileFs

  where go cid (a, valid, p) = do
          when valid $ do
            moveCursor a 1
            drawString p
