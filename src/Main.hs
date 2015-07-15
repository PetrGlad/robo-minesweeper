{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import qualified System.Random as SR
import qualified Control.Monad.Random as CMR

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.MultiMap as Mm
import qualified Data.MultiSet as Ms
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Tuple as Tu
import qualified Control.Monad as Cm
-- import Data.Sequence

import System.Console.ANSI

-- import qualified Control.Exception as CE
-- import System.IO
-- import qualified Control.Concurrent as CC

type Pos = (Int, Int)
type Size = Pos
type PosDelta = Pos
type Mines = Set Pos
type Layer a = Map Pos a
type Intel = Layer Int
data Cell = CMine
            | CUnknown
            | CFree
       deriving (Eq)
type Field = Layer Cell

genMine :: (SR.RandomGen g) => (Int, Int) -> CMR.Rand g (Int, Int)
genMine (maxX, maxY) = do
    x <- CMR.getRandomR (0, maxX - 1)
    y <- CMR.getRandomR (0, maxY - 1)
    return (x, y)

genMines :: Size -> Int -> IO Mines
genMines size cnt = do
      mines <- CMR.evalRandIO $ sequence $ replicate cnt  $ genMine size
      return $ S.fromList mines

genField :: Size -> Field
genField (sx, sy) = M.fromList [((x,y), CUnknown) | x <- [0..(sx-1)], y <- [0..(sy-1)]]

renderCell :: Show a => Pos -> a -> IO ()
renderCell (x, y) c = do
               setCursorPosition y x
               putStr (show c)

renderLayer :: Show a => Map Pos a -> IO ()
renderLayer cells = mapM_ (uncurry renderCell) (M.toList cells)

renderColored :: [SGR] -> IO () -> IO ()
renderColored colorSetup action = do
  setSGR colorSetup
  action
  setSGR [Reset]

positionsToLayer :: (Show a) => (Pos -> a) -> Set Pos -> Layer a
positionsToLayer showFn = M.fromSet showFn

instance Show Cell where
  show CMine = "@"
  show CUnknown = "#"
  show CFree = " "

isGameComplete :: Mines -> Field -> Bool
isGameComplete mines field = S.size mines == L.length (filter (CUnknown==) (M.elems field))

step :: Mines -> Field -> Pos -> Maybe Field
step mines field pos
  | S.member pos mines = Nothing
  | otherwise = Just $ newField
  where newField :: Map Pos Cell
        newField = M.alter (const (Just CFree)) pos field

neighbourDeltas :: [PosDelta]
neighbourDeltas = [(x, y) | x <- d, y <- d, x /= 0 || y /= 0]
  where d = [-1..1]

shiftPos :: Pos -> PosDelta -> Pos
shiftPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nearPositions :: Pos -> [Pos]
nearPositions pos = fmap (\d -> shiftPos pos d) neighbourDeltas

genIntel :: Mines -> Intel
genIntel mines = foldl updateNeighbours M.empty (S.toList mines)
  where updateNeighbours intel pos = foldl (\intl nPos -> M.alter updateCount nPos intl)
                                           intel (nearPositions pos)
        updateCount Nothing = Just 1
        updateCount (Just x) = Just (x + 1)

-- The cell is already known as not containing mine
isFreeCell :: Field -> Pos -> Bool
isFreeCell field pos = (M.lookup pos field) == (Just CFree)

filterLayer :: Layer a -> Field -> Layer a
filterLayer intel field = M.filterWithKey (\pos _ -> isFreeCell field pos) intel

-----------------------------------------------------------

chooseProbePosition :: Size -> Field -> Intel -> Pos
chooseProbePosition fieldSize field intel = choice
  where
    -- Fallback implementation
    unknowns = M.toList $ M.filter (CUnknown==) field
    choice0 = head $ fmap fst unknowns

    -- TODO Implement
    intelRel = intelMatrix fieldSize (enumPositions fieldSize)
    edgeMinesMatrix = discoverableMinesMatrix (visibleIntelMatrix intelRel field) field
    neighbourToMine = groupBySecond edgeMinesMatrix
    mineToNeighbour = groupByFirst edgeMinesMatrix
    choice =
      if Mm.null neighbourToMine
        then choice0
        else (head . snd . head . M.toList . Mm.toMap) neighbourToMine -- Stub -- TODO Implement
        -- else
        -- consistentCombinations edgeMinesMatrix intel

type CellPair = (Pos, Pos)

enumPositions :: Size -> [Pos]
enumPositions (width, height) = [(col, row) | col <- [0..(width-1)], row <- [0..(height-1)]]

inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = x >= lo && x < hi

inBoard :: Size -> Pos -> Bool
inBoard (w, h) (x,y) = (inRange 0 w x) && (inRange 0 h y)

-- Produces (cell, neighbour) relations
intelMatrix :: Size -> [Pos] -> [CellPair]
intelMatrix s ps = concatMap (\p -> fmap (\n -> (p, n))
                                         (filter (inBoard s) (nearPositions p)))
                             ps

-- Limit relations to ones we can reason about on given field
-- (i.e. a relation neighbour is visible/uncovered)
visibleIntelMatrix :: [CellPair] -> Field -> [CellPair]
visibleIntelMatrix pairs field = filter ((isFreeCell field) . snd) pairs

discoverableMinesMatrix :: [CellPair] -> Field -> [CellPair]
discoverableMinesMatrix pairs field = filter (not . (isFreeCell field) . fst) pairs

groupByFirst :: Ord a => [(a, b)] -> Mm.MultiMap a b
groupByFirst = foldl (\mm (k,v) -> Mm.insert k v mm) Mm.empty

groupBySecond :: [CellPair] -> Mm.MultiMap Pos Pos
groupBySecond = groupByFirst . (map Tu.swap)

getIntel :: Intel -> Pos -> Int
getIntel i p = case M.lookup p i of
                 Just x -> x
                 Nothing -> 0

-- pos->True mine, pos->False no mine
-- (Fixes free cells in combination as well as taken ones
-- since any changes to chosen set will make it inconsistent.
-- So we do not change existing layout during test)
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

-- Find some mine combinations that are consistent with known Intel
-- (Map Neighbour [Mine positions]) -> intel -> consistent mine placements
-- (XXX The algorithm in connectedSets can be optimized for speed)
consistentCombinations :: [CellPair] -> Intel -> [MineLayout]
consistentCombinations edgeRelations intel =
  concatMap (\ns -> constrainedCombos M.empty (S.toList ns)) connectedSets
  where
    neighbourToMine = groupBySecond $ edgeRelations
    mineToNeighbour = groupByFirst $ edgeRelations

    -- (Map MinePosition [Neighbours]) -> Neighbour/intel cell -> list of other Neighbour/intel cells affected
    getLinkedNeighbours :: Pos -> [Pos]
    getLinkedNeighbours pos = Mm.lookup pos mineToNeighbour

    linkedMinePoss p = (Mm.lookup p neighbourToMine)
    linkedNeighbours p = S.fromList $ concatMap getLinkedNeighbours (linkedMinePoss p)

    -- (Likely connectedSets could be implicitly calculated in constrainedCombos - but that would complicate code)
    connectedSets :: [Set Pos]
    connectedSets = split (S.toList (M.keysSet (Mm.toMap neighbourToMine))) []
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

    -- (A) -- 1. List of linked neighbours.
    -- -- 2. Key set of accumulated MineLayout - to see which positions are already taken while generating new layouts.
    -- -- 3. neighbourToMine - to generate consistent layouts together with 2.
    constrainedCombos :: MineLayout -> [Pos] -> [MineLayout]
    -- Seed testPositions with single item list. testPositions - neigbours to be satisfied by intel
    constrainedCombos currentLayout [] = [currentLayout]
    constrainedCombos currentLayout (testPos : tps) =
        -- Generate only combinations that are consistent in this position.
        if isFeasible
          then map (\c -> M.union currentLayout c)
                   (concatMap testCombo tpLayouts)
          else []
        where
          tpIntel = getIntel intel testPos
          -- Find related mine positions that are already in layout by splitting into (taken, available)
          lmp = L.partition (\p -> M.member p currentLayout) (linkedMinePoss testPos)
          takenMps = (fst lmp)
          availableMps = (snd lmp)
          takenCount = L.foldl (\s x -> s +
                                  (boolTo01 (M.findWithDefault False x currentLayout)))
                               0
                               takenMps
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
          testCombo posLayout = constrainedCombos (M.union currentLayout posLayout) tps

-- permutations n = sequence . replicate n
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = Ms.toOccurList . Ms.fromList

-- http://stackoverflow.com/a/22577148/117220
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | otherwise = case l of
             [] -> []
             (y : ys) -> map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys

-- Binomial coefficient
-- http://stackoverflow.com/a/6806997/117220
choiceCount :: Int -> Int -> Int
choiceCount _n 0 = 1
choiceCount 0 _k = 0
choiceCount n k = choiceCount (n-1) (k-1) * n `div` k

-- It would be better to have this in multimap lib
instance (Show a, Show b) => Show (Mm.MultiMap a b) where
  show = show . Mm.toMap

-----------------------------------------------------------

moveCursorBelow :: Size -> IO ()
moveCursorBelow boardSize = setCursorPosition (snd boardSize) 0

showStatus :: Size -> String -> IO ()
showStatus fieldSize message = do
    moveCursorBelow fieldSize
    putStrLn message

renderBoard :: Field -> Intel -> Mines -> IO ()
renderBoard field intel mines =
  let minesColor = [SetConsoleIntensity BoldIntensity,
                    SetColor Foreground Vivid Red]
      fieldColor = [SetColor Foreground Dull White]
  in do
    renderColored fieldColor $ renderLayer field
    renderColored minesColor $ renderLayer $ positionsToLayer (const CMine) mines
    renderLayer intel

gameStep :: Size -> Mines -> Field -> IO ()
gameStep fieldSize mines field =
  let intel = filterLayer (genIntel mines) field
      minesCount = S.size mines
      ------------------------------------------
--       probePos = (chooseProbePosition fieldSize field intel)
--       newField = step mines field probePos
      showFinalStatus message = do
        showStatus fieldSize message
        return ()
  in do
    clearScreen
    renderBoard field intel mines
    moveCursorBelow fieldSize -- Move it away so it does not obstruct cells

    -------------------------
    let intelRel = intelMatrix fieldSize (enumPositions fieldSize)
    let viMatrix = (visibleIntelMatrix intelRel field)
--     putStrLn $ show $ viMatrix
    let edgeRelations = discoverableMinesMatrix viMatrix field -- "Edge" between explored and unknown cells
--     putStrLn $ show $ edgeRelations
    let combos = consistentCombinations edgeRelations intel
--     putStrLn $ show $ combos

    -- TODO Implement mask with found mines for optimization and estimation remaining mine count in heuristics.
    -- TODO Implement 0 intel shortcut - do not do full analysis on them (can we make it part of generic algorithm?)
    -- TODO Implement mines "disarm" to reduce unknown boundary (place a mark on mine and subtract this mine from intel)

    -- If probability of mine on "inner" unexplored cell is less than on the edge then choose one such
    -- cell randomly.
    -- Choose "step into unknown" position:
    let unknownMargin = Mm.keysSet (groupByFirst edgeRelations)
    let farField = S.toList $ M.keysSet $ M.filterWithKey (\p c -> (c == CUnknown) && (not $ S.member p unknownMargin)) field
    let farPoss = case farField of
                        [] -> []
                        (x:_xs) -> [(x, fromIntegral minesCount -- FIXME - here should be number of remaining mines
                         / (fromIntegral $ L.length farField))]


    let freqGroups = Mm.toMap $ Mm.fromList $ fmap (\(p,t) -> (p, boolTo01 t))
                     $ L.concat $ fmap M.toList combos

    -- freqs :: [(Pos, Float)]
    let freqs = L.sortBy (\(_p1,f1) (_p2,f2) -> compare f1 f2)
                $ L.concat [
                    (M.toList
                       $ M.map (\cs -> ((fromIntegral (sum cs))::Float) / (fromIntegral (L.length combos)))
                       freqGroups)
                    -- TODO Implement , farPoss
                    ]

    putStrLn ""
    putStrLn ""
--     putStrLn $ show $ L.length combos
--     putStrLn $ show $ L.take 4 combos
    putStrLn $ show $ L.take 30 freqs

    -- noMines are sure steps so we'll do them in batch.
    let noMines = takeWhile ((0==) . snd) freqs
    let probePoss = fmap fst $
          case noMines of
            [] -> (case freqs of
                     [] -> [((0,0), 1)]
                     (x:_xs) -> [x])
            x -> x

    let newField = Cm.foldM (step mines) field probePoss

    -- CC.threadDelay 300000
    -- _ <- getChar

    -------------------------

    case newField of
      Nothing -> showFinalStatus ("Tripped on mine at " ++ (show probePoss))
      Just f ->
        if isGameComplete mines f
          then do
                renderBoard f intel mines
                showFinalStatus "Done"
          else gameStep fieldSize mines f

main :: IO ()
main =
  let dims = (128, 64)
  in do
    clearScreen
    mines <- genMines dims 1500 -- 350
    gameStep dims mines (genField dims)


-- Tests
-- 7x6
-- "001###@
-- 013@@@#
-- 02@@###
-- 13@####
-- #@2#@##
-- #######"
-- > Actual: Tripped on mine at (1,4). Expected: Could be safely probing (3,0)
