{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module File where

import Format.SplitTime

import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap          as IM
import qualified Data.List            as L

import Codec.Compression.GZip
import Control.Exception
import Data.Binary
import Data.IntMap   (IntMap)
import Data.List     (intersperse)
import Data.Map      (Map)
import Data.Maybe
import Data.Text     (Text)
import Data.Time.Clock


type TagMap = [(Text, Int)]
type Run    = IntMap NominalDiffTime

data FileFormat = FileFormat TagMap TagMap (IntMap Run)
                deriving (Show)

instance Binary FileFormat where
  put (FileFormat _ t rs) = put t >> (put $ IM.map (IM.map fromEnum) rs)
  get = FileFormat <$> pure [] <*> get <*> (IM.map (IM.map toEnum) <$> get)


mkRun :: FileFormat -> [NominalDiffTime] -> [Bool] -> Maybe Run
mkRun (FileFormat ls _ _) times valid =
  let t' = if length times == length ls -- If we didn't finish the run, don't record the last split
             then times
             else init times
      r = filter fst $ zip valid $ zip (map snd ls) t'
  in if null r
       then Nothing
       else Just $ IM.fromList $ map snd r


recordRun :: FileFormat -> [NominalDiffTime] -> [Bool] -> FileFormat
recordRun f _  [] = f
recordRun f [0] _ = f
recordRun f@(FileFormat ls m rs) times valid =
  let run = mkRun f times valid
      key = if null rs
              then 0
              else succ $ fst $ IM.findMax rs
  in case run of
       Nothing  -> f
       (Just r) -> FileFormat ls m (IM.insert key r rs)


save :: FileFormat -> IO ()
save = B.writeFile "splits.dat" . compress . encode


load :: [Text] -> IO FileFormat
load levels = do
  f <- load'

  return $ mergeLevelNames levels f


load' :: IO FileFormat
load' = do
  (FileFormat _ l r) <- catch (B.readFile "splits.dat" >>= return . decode . decompress)
                        (\(e :: IOException) -> return $ FileFormat [] [] IM.empty)
  return $ FileFormat l l r


mergeLevelNames :: [Text] -> FileFormat -> FileFormat
mergeLevelNames levels (FileFormat _ m rs) =
  let (FileFormat nl nm nrs) = foldr go (FileFormat [] (reverse m) rs) $ reverse levels
  in FileFormat (reverse nl) (reverse nm) nrs
  where go :: Text -> FileFormat -> FileFormat
        go l (FileFormat ls m rs) =
          let (ln, nm) = case L.lookup l m of
                           (Just n) -> ((l, n), m)
                           Nothing  ->
                             let nn = (l, succ $ foldr max 0 $ map snd m)
                             in (nn, nn:m)
          in FileFormat (ln:ls) nm rs


levelData :: FileFormat -> [(Int, [NominalDiffTime])]
levelData (FileFormat ls _ rm) =
  let rs = IM.elems rm
      sideways = map (\k -> (k, mapMaybe (IM.lookup k) rs)) $ map snd ls
  in sideways


sumOfBests :: FileFormat -> NominalDiffTime
sumOfBests f =
  let lData = levelData f
  in if any (null . snd) lData
       then toEnum 0
       else sum $ map (minimum . snd) lData


personalBest :: FileFormat -> NominalDiffTime
personalBest (FileFormat ls _ rs) =
  let rs' = filter ((== length ls) . IM.size) $ IM.elems rs
  in if null rs'
       then toEnum 0
       else minimum $ map sum rs'


levelBests :: FileFormat -> [(Int, NominalDiffTime)]
levelBests f =
  let lData   = levelData f
      bests   = map (minimum . (toEnum maxBound:) . snd) lData
      levels  = map fst lData
  in zip levels bests


playTime :: FileFormat -> NominalDiffTime
playTime (FileFormat _ ls rs) =
  let lData = levelData $ FileFormat ls ls rs
  in sum $ map (sum . snd) lData


output :: FileFormat -> String
output (FileFormat _ ls rs) =
  let levels = map snd $ levelData $ FileFormat ls ls rs
      lengths = map length levels
      n     = "N <- " ++ (show $ length lengths)
      num_y = "num_y <- c( " ++ (concat $ intersperse ", " $ map show lengths) ++ " )"
      y     = "y <- c( " ++ (concat $ intersperse ", " $ map (show . realToFrac) $ concat levels) ++ " )"
  in unlines [n, num_y, y]
