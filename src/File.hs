{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiWayIf #-}
module File where

import Format.SplitTime

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap          as IM
import qualified Data.List            as L
import qualified Data.Text            as T

import Codec.Compression.GZip
import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Exception
import Data.Binary
import Data.Binary.Get (lookAheadM, getByteString)
import Data.Binary.Put (putByteString)
import Data.IntMap   (IntMap)
import Data.List     (intersperse)
import Data.Map      (Map)
import Data.Maybe
import Data.Text     (Text)
import Data.Time.Clock
import Text.Printf


-- Various descriptive type synonyms
type TagMap         = [(Text, Int)]
type AllKnownLevels = TagMap
type CurrentLevels  = TagMap

type Run = IntMap (NominalDiffTime, Bool)


listRuns :: FileFormat -> IO ()
listRuns (FileFormat _ levels runMap) =
  let levelLookup = map (\(a, b) -> (b, a)) levels
      runs = IM.toAscList runMap
  in mapM_ (\(key, r) -> do
               let run = IM.toAscList r
                   names = map (fromJust . flip lookup levelLookup . fst) run
                   longestName = maximum $ map T.length names
                   namedRuns = zip (zip (map fst run) names) $ map snd run
               putStrLn $ "\nRun #" ++ show key
               mapM_ (\((n, nt), (time, valid)) -> putStrLn $ (printf "%3d %-*s %s" n longestName nt (show $ splitTime time)) ++ if valid then "" else "(i)") namedRuns
           ) runs

deleteRun :: FileFormat -> Int -> FileFormat
deleteRun (FileFormat cl al rs) r = FileFormat cl al $ r `IM.delete` rs

--        putStr $ unlines $ zipWith (\a b -> printf "%-*s" longestName a ++ " - " ++ show (splitTime b)) lNames bests

-- Version tag data
tagLength :: Int
tagLength = 15

-- The first tagged version
v200Tag :: BS.ByteString
v200Tag = "splitsDatV2.0.0"

currentTag :: BS.ByteString
currentTag = v200Tag

-- The internal data format for loading, saving, and manipulating split data.
data FileFormat = FileFormat CurrentLevels AllKnownLevels (IntMap Run)
                deriving (Show)

instance Binary FileFormat where
  -- Output code always outputs the newest version. We won't support outputting to previous versions
  -- of the data format.
  put (FileFormat _ t rs) = putByteString currentTag
                         >> put t
                         >> put (IM.map (IM.map (first fromEnum)) rs)
  get = do
        -- V1 only saved valid splits, so we have to add a "True" to get it in the current data format
        -- V1 didn't have a version tag, so it's relegated to the backup loading
    let v1Loader = FileFormat <$> pure [] <*> get <*> (IM.map (IM.map (flip (,) True . toEnum)) <$> get)
        -- There are, paradoxically, two versions of V2, one with and one without the version tag
        -- The tagged version is loaded by the lookAhead loader, as all future versions will be.
        -- The untagged version is loaded in the backup loading section, along with V1.
        v2Loader = FileFormat <$> pure [] <*> get <*> (IM.map (IM.map (first toEnum)) <$> get)

    -- Pull the appropriate number of bytes for the tag, then try to match it to a known tag
    -- If no tag is matched, the bytes read are returned so we can try again
    loaded <- lookAheadM $ do
      tag <- getByteString tagLength

      if | tag == v200Tag -> Just <$> v2Loader
         | otherwise      -> pure Nothing

    -- Evaluates to a FileFormat read by the tag code above or tries a backup loader (in left to
    -- right order). If neither loads, `binary` provides an error.
    case loaded of
      (Just a) -> pure a
      Nothing  -> v2Loader <|> v1Loader


-- Tries to process provided time and validity data into a Run.
--
-- Relies on the times, validities, and levels contained in 'ls' (i.e., those values passed to
-- `mergeLevelNames`) being in the same order.
mkRun :: FileFormat -> [NominalDiffTime] -> [Bool] -> Maybe Run
mkRun (FileFormat ls _ _) times valid =
  let t' = if length times == length ls -- If we didn't finish the run, don't record the last split
             then times
             else init times
      r = filter ((> 0) . fst . snd) $ zip (map snd ls) $ zip t' valid
  in if null r
       then Nothing
       else Just $ IM.fromList r


-- Tries to add a new Run to a FileFormat, given times and their validities
--
-- If for any reason a valid Run can not be built, this function is equivalent to `id`
recordRun :: FileFormat -> [NominalDiffTime] -> [Bool] -> FileFormat
recordRun f _  [] = f
recordRun f [0] _ = f
recordRun f@(FileFormat ls m rs) times valid =
  let run = mkRun f times valid

      -- Sets the run counter to either one past the current max, or zero if there are no runs yet
      key = if null rs
              then 0
              else succ $ fst $ IM.findMax rs
  in case run of
       Nothing  -> f
       (Just r) -> FileFormat ls m (IM.insert key r rs)


-- Saves "splits.dat", compressing with ZLib and overwriting any existing file.
save :: FileFormat -> IO ()
save = B.writeFile "splits.dat" . compress . encode


-- Convenience function which loads the data file and immediately runs `mergeLevelNames`.
load :: [Text] -> IO FileFormat
load levels = do
  f <- load'

  return $ mergeLevelNames levels f


-- Loads the data file, "splits.dat", returning an empty FileFormat on error.
load' :: IO FileFormat
load' = do
  (FileFormat _ l r) <- catch (B.readFile "splits.dat" >>= return . decode . decompress)
                        (\(e :: IOException) -> return $ FileFormat [] [] IM.empty)
  return $ FileFormat l l r


-- Takes a list of level names and restricts the FileFormat to consider only those levels for some
-- operations (e.g., anything that calls levelData)
mergeLevelNames :: [Text] -> FileFormat -> FileFormat
mergeLevelNames levels (FileFormat _ m rs) =
  let (FileFormat nl nm nrs) = foldr go (FileFormat [] (reverse m) rs) $ reverse $ canonicalize levels
  in FileFormat (reverse nl) (reverse nm) nrs
  where go :: Text -> FileFormat -> FileFormat
        go l (FileFormat ls m rs) =
          let (ln, nm) = case L.lookup l m of
                           (Just n) -> ((l, n), m)
                           Nothing  ->
                             let nn = (l, succ $ foldr max 0 $ map snd m)
                             in (nn, nn:m)
          in FileFormat (ln:ls) nm rs


-- Turns the FileFormat "sideways", such that the output data format is sorted by levels, rather
-- than by runs.
--
-- Only considers levels that are currently valid (e.g., what's present in the YAML file).
levelData :: FileFormat -> [(Int, [(NominalDiffTime, Bool)])]
levelData (FileFormat ls _ rm) =
  let rs       = IM.elems rm
      sideways = map (\k -> (k, mapMaybe (IM.lookup k) rs)) $ map snd ls
  in sideways


-- Restricts the output of levelData to only include valid splits.
onlyValidSplits :: [(Int, [(NominalDiffTime, Bool)])] -> [(Int, [NominalDiffTime])]
onlyValidSplits ls = map (second (map fst . filter snd)) ls


-- Looks at all the valid splits for each level, finds the smallest, and evaluates to the sum of
-- those shortest times.
--
-- If any split has no valid times, we can't provide a valid sumOfBests, so we evaluate to 0
sumOfBests :: FileFormat -> NominalDiffTime
sumOfBests f =
  let lData = onlyValidSplits $ levelData f
  in if any (null . snd) lData
       then toEnum 0
       else sum $ map (minimum . snd) lData


-- Evaluates to the shortest time taken in any single complete run.
--
-- Includes runs with invalid splits, as it is assumed the sum of splits in a run is the true total
-- length of the run, regardless of split validity.
personalBest :: FileFormat -> NominalDiffTime
personalBest = minimum . completeRuns


completeRuns :: FileFormat -> [NominalDiffTime]
completeRuns (FileFormat ls _ rs) =
  let rs' = filter ((== length ls) . IM.size) $ IM.elems rs
  in if null rs'
       then []
       else map (sum . IM.map fst) rs'


-- Reports the shortest valid time for every split
levelBests :: FileFormat -> [(Int, NominalDiffTime)]
levelBests f =
  let lData   = onlyValidSplits $ levelData f
      bests   = map (minimum . (toEnum maxBound:) . snd) lData
      levels  = map fst lData
  in zip levels bests


-- Reports the sum of all splits, including invalid splits (which are assumed to be real play time,
-- even if they aren't valid representations of the quantity of time needed to complete that split).
playTime :: FileFormat -> NominalDiffTime
playTime (FileFormat _ ls rs) =
  let lData = levelData $ FileFormat ls ls rs
  in sum $ map (sum . map fst . snd) lData


-- Outputs data for all valid splits in a format readable by the `unisplits` STAN model.
output :: FileFormat -> String
output f@(FileFormat ls _ rs) =
  let levels          = map snd $ onlyValidSplits $ levelData $ FileFormat ls ls rs
      scaledByMinimum = map (\ls -> (1.0e-6:) $ filter (> 0) $ map (subtract 1 . (/ (minimum ls))) ls) levels
      lengths         = map length scaledByMinimum

      n     = "N <- " ++ (show $ length lengths)
      num_y = "num_y <- c( " ++ (concat $ intersperse ", " $ map show lengths) ++ " )"
      y     = "y <- c( " ++ (concat $ intersperse ", " $ map (show . realToFrac) $ concat scaledByMinimum) ++ " )"
      mins  = "mins <- c( " ++ (concat $ intersperse ", " $ map (show . realToFrac . (\a -> if a == toEnum maxBound then 0 else a) . minimum . (toEnum maxBound:)) levels) ++ " )"
      nFull_runs  = "num_runs <- " ++ (show $ length $ completeRuns f)
      full_runs = "full_runs <- c( " ++ (concat $ intersperse ", " $ map (show . realToFrac) $ completeRuns f) ++ " )"
  in unlines [n, num_y, y, mins, nFull_runs, full_runs]

canonicalize :: [Text] -> [Text]
canonicalize ss = map snd $ scanl go ([head ss], head ss) $ tail ss
  where go :: ([Text], Text) -> Text -> ([Text], Text)
        go (conts, _) s =
          let l      = 2 * length conts
              prefL  = max 0 l - 1
              prefix = T.pack $ replicate l ' '
              deeper = (T.drop prefL s) : conts
              canon  = T.concat $ reverse deeper
          in if prefix `T.isPrefixOf` s
               then (deeper, canon)
               else if null conts
                      then ([s], s)
                      else go (tail conts, s) s
