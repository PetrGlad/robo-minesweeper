{-# OPTIONS_GHC -Wall #-}
-- | Algorithm that decides what positions to test next
module Algorithm where

import Common

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as Mm
import qualified Data.MultiSet as Ms
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Tuple as Tu
-- import qualified Control.Monad as Cm
-- import Data.Sequence
-- import Control.Parallel
import Control.Parallel.Strategies (parMap, rseq)

import Debug.Trace

type CellPair = (Pos, Pos)

notShorterThan :: Int -> [a] -> Bool
notShorterThan n l = n == length (take n l)

{- Default algorithm, returns set ofpositions to probe next.
  The algorithm:
  1. Find all possible mine layouts that satisfy available intel.
  2. Rank involved positions by probability (frequency in all layouts) of mine being there.
  3. Pick positions for probe/mark according to calculated frequency.

  Optimization step:
  On subsequent calls, once mine is marked it is subtracted from intel and explicitly excised
  from probe positions. Latter is required since intel has "hole" right under mine so we need
  to handle a special case. (Need to do performance benchmarks to prove that it subtraction is effective).
 -}
choosePositions :: Algorithm
choosePositions fieldSize field intel =
  choosePositionsByFrequency (edgeRelations fieldSize field) fieldSize field intel

{- Alternative version of choosePositions.

  Intention was to avoid full analysis if there are safe probe positions.
  In fact this version is slower and may accumulate too big chunks of analysis for later.
  However it produces curious behavior.
-}
choosePositions2 :: Algorithm
choosePositions2 fieldSize field intel =
  if not (null safeProbes)
    then (safeProbes, [])
    else choosePositionsByFrequency edge fieldSize field intel
  where
    edge = edgeRelations fieldSize field
    safeProbes = findSafeProbes edge intel

filterFst :: (a -> Bool) -> [(a, b)] -> [(a, b)]
filterFst pred = filter (\(x, _) -> pred x)

choosePositionsByFrequency :: [CellPair] -> Size -> Field -> Intel -> ([Pos], [Pos])
choosePositionsByFrequency edge fieldSize field intel =
  (probePositions, foundMines)
  where
    disarmed = filterField CDisarmed field
    isNotDisarmed p = not $ S.member p disarmed
    -- Filtering edge splits connected sets into smaller pieces which reduces number of combinations to consider
    freqs = rankProbePositions (filterFst isNotDisarmed edge) (subtractMines disarmed intel)
    probePositions = pickProbePoss field (filterFst isNotDisarmed freqs)
    foundMines = fmap fst $ filter ((1==) . snd) freqs

{- Pick position(s) to probe given mine frequencies.
   Precondition: Freqs should be ordered by increasing frequency.
-}
pickProbePoss :: Field -> [(Pos, Float)] -> [Pos]
pickProbePoss field freqs = case pick of
  [] -> fallback
  x -> x
  where
    pick = fmap fst $
      case takeWhile ((0==) . snd) freqs of
        [] -> (case freqs of
               [] -> [] -- Can start anywhere
               (x@(_pos,freq):_xs) | freq < 1 -> [x] -- Pick one with least probability of mine being there
               _ -> []) -- Only sure mines in the fringe positions
        x -> x -- Positions without mines. These are sure steps so we pick them in batch for optimization.

    {- TODO If probability of mine on an "inner" unexplored cell is less than on the edge then choose one such cell randomly.
    -- (This should increase chance of success in ambiguous situations)
    -- Choose "step into unknown" position:    -- farPoss = case farField of
    --             [] -> []
    --             (x:_xs) -> [(x, fromIntegral minesCount -- FIXME - here should be number of remaining mines
    --                         / (fromIntegral $ L.length farField))]
    -}
    -- Fallback implementation: Get an unknown position but not in fringe (we're here because fringe is all mines)
    fallback = take 1 $ S.toList $ S.difference
      (filterField CUnknown field)
      (S.fromList (fmap fst freqs))

edgeRelations :: Size -> Field -> [CellPair]
edgeRelations fieldSize field = discoverableMinesMatrix
    (visibleIntelMatrix intelRel field)
    field
  where intelRel = intelMatrix fieldSize (enumPositions fieldSize)

-- Return "mine frequencies" [(mine-position, probability-of-mine-there)] oredered by increasing probability
rankProbePositions :: [CellPair] -> Intel -> [(Pos, Float)]
rankProbePositions edge intel =
    L.sortBy (\(_p1, f1) (_p2, f2) -> compare f1 f2)
    $ (M.toList
       $ M.map (\cs -> ((fromIntegral (sum cs))::Float) / (fromIntegral (L.length cs)))
           posOptions)
  where
    combos = consistentCombinations edge intel
    -- Possible values (mine/free) of positions
    posOptions = Mm.toMap $ Mm.fromList $ fmap (\(p,t) -> (p, boolTo01 t))
                 $ L.concatMap M.toList combos

-- Optimization shortcut: Pick probe positions around zero intel cells to skip full combination analysis.
findSafeProbes :: [CellPair] -> Intel -> [Pos]
findSafeProbes edge intel = {-S.toList $ S.fromList $-} map fst $ filter
    (\(_, n) -> not $ S.member n dangerIntelPoss)
    edge
  where dangerIntelPoss = M.keysSet $ M.filter (/=0) intel

-- Ignore already found mines in intel
subtractMines :: Set Pos -> Intel -> Intel
subtractMines mines intl = foldl
     (\i dm -> M.alter decIntl dm i) intl
     (concatMap nearPositions $ S.toList mines)
  where
    decIntl = (\mcnt -> case mcnt of
                         Nothing -> Nothing
                         Just c | c < 1 -> decErr
                         Just c | c == 1 -> Nothing
                         Just c -> Just $ c - 1)
    decErr = error "Wrong disarmed mine (or intl is inconsistent)"

inBoard :: Size -> Pos -> Bool
inBoard (w, h) (x,y) = (inRange 0 w x) && (inRange 0 h y)

-- Produces ((mine)cell, neighbour) relations
intelMatrix :: Size -> [Pos] -> [CellPair]
intelMatrix s ps = concatMap (\p -> fmap (\n -> (p, n))
                                         (filter (inBoard s) (nearPositions p)))
                             ps

-- Limit relations to ones we can reason about on given field
-- (i.e. a relation neighbour is visible/uncovered)
visibleIntelMatrix :: [CellPair] -> Field -> [CellPair]
visibleIntelMatrix pairs field = filter ((isFreeCell field) . snd) pairs

{- Return edge relatios where mines can be discovered.
   First position in each pair refers to (possible) mine position, second one to discovered/free position
-}
discoverableMinesMatrix :: [CellPair] -> Field -> [CellPair]
discoverableMinesMatrix pairs field = filter (not . (isFreeCell field) . fst) pairs

groupByFirst :: Ord a => [(a, b)] -> Mm.MultiMap a b
groupByFirst = foldl (\mm (k,v) -> Mm.insert k v mm) Mm.empty

groupBySecond :: Ord b => [(a, b)] -> Mm.MultiMap b a
groupBySecond = groupByFirst . (map Tu.swap)

getIntel :: Intel -> Pos -> Int
getIntel i p = case M.lookup p i of
                 Just x -> x
                 Nothing -> 0

-- pos->True mine, pos->False no mine
type MineLayout = Map Pos Bool

justIsTrue :: Maybe a -> Bool
-- Every program should ensure this.
justIsTrue x = case x of
                 Just _ -> True
                 Nothing -> False

boolTo01 :: Bool -> Int
boolTo01 x = case x of
              True -> 1
              False -> 0

-- (TODO (?) Optimization: Likely connectedSets could be implicitly calculated in constrainedCombos - but that would complicate code)
connectedSets :: Mm.MultiMap Pos Pos -> (Pos -> Set Pos) -> [Set Pos]
connectedSets neighbourToMine linkedNeighbours =
  split (S.toList (M.keysSet (Mm.toMap neighbourToMine))) []
  where
    split :: [Pos] -> [Set Pos] -> [Set Pos]
    split [] sets = sets
    split (n : ns) sets =
      let related = getConnected (S.singleton n)
          unrelated = L.filter (\x -> not (S.member x related)) ns
      in split unrelated (related : sets)

    getConnected :: Set Pos -> Set Pos
    getConnected s =
      let newSet = S.unions $ fmap linkedNeighbours (S.toList s)
      in if (S.size newSet) == (S.size s)
           then s
           else getConnected newSet

-- Return list of mine combinations that satisfy intel
constrainedCombos :: Intel -> (Pos -> [Pos]) -> MineLayout -> [Pos] -> [MineLayout]
-- Seed testPositions with single item list. testPositions - neigbours to be satisfied by intel
-- currentLayout fixes free cells as well as taken ones since any changes to already chosen set
-- will make it inconsistent.
--  So we do not change existing layout while analysing new positions in constrainedCombos)
constrainedCombos _intel _linkedMinePoss currentLayout [] = [currentLayout]
constrainedCombos intel linkedMinePoss currentLayout (testPos : tps) =
    -- Generate only combinations that are consistent in this position.
    if isFeasible
      then map (\c -> M.union currentLayout c)
               (L.concat $ (parMap rseq) testCombo tpLayouts)
      else []
    where
      tpIntel = getIntel intel testPos
      -- Find related mine positions that are already in layout by splitting into (taken, available)
      takenAndAvailable = L.partition (\p -> M.member p currentLayout) (linkedMinePoss testPos)
      takenMps = (fst takenAndAvailable)
      availableMps = (snd takenAndAvailable)
      takenCount = L.foldl (\s x -> s +
                              (boolTo01 (M.findWithDefault False x currentLayout)))
                           0 takenMps
      isFeasible = (tpIntel - takenCount) <= (L.length availableMps)
      tpCombinations :: [[Pos]]
      tpCombinations = combinations (tpIntel - takenCount) availableMps
      -- Convert combination of present mines to mine layout:
      tpLayouts :: [MineLayout]
      tpLayouts = map
        (\tpc -> M.fromList $ fmap (\x -> (x, L.elem x tpc)) availableMps)
        tpCombinations

      -- In case of cycle we should just get empty combo and move on
      -- Recursive call returning some consistent layout:
      testCombo :: MineLayout -> [MineLayout]
      testCombo posLayout =
        constrainedCombos intel linkedMinePoss (M.union currentLayout posLayout) tps

{- Find mine combinations that are consistent with known Intel
 (Map Neighbour [Mine positions]) -> intel -> consistent mine placements

 Note that there can be sets of placements that do not affect each other.
 This function returns elemts of such sets without combining them into larger placements.
 Ihat is if there are positions [a,b] and [c,d] that are not connected then their placements
 are not combined into pacements for [a,b,c,d].
-}
consistentCombinations :: [CellPair] -> Intel -> [MineLayout]
consistentCombinations edge intel =
  concatMap (\ns -> constrainedCombos intel linkedMinePoss M.empty (S.toList ns))
            (connectedSets neighbourToMine linkedNeighbours)
  where
    neighbourToMine = groupBySecond $ edge
    mineToNeighbour = groupByFirst $ edge
    linkedMinePoss = (flip Mm.lookup neighbourToMine)
    -- Neighbours that share common mine places with this one
    linkedNeighbours p = S.fromList $ concatMap
                           (flip Mm.lookup mineToNeighbour) -- (get list of other Neighbour/intel cells affected by given position)
                           (linkedMinePoss p)

-- permutations n = sequence . replicate n
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = Ms.toOccurList . Ms.fromList

-- Return all combinations of k elements from xs (based on http://stackoverflow.com/a/22577148/117220)
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | otherwise = case l of
             [] -> []
             (y : ys) -> map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys

-- Binomial coefficient (from http://stackoverflow.com/a/6806997/117220)
choiceCount :: Int -> Int -> Int
choiceCount _n 0 = 1
choiceCount 0 _k = 0
choiceCount n k = choiceCount (n-1) (k-1) * n `div` k
