{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as Mm
import Data.Set (Set)
import qualified Data.Set as S

type Pos = (Int, Int)
type Size = Pos
type PosDelta = Pos
type Mines = Set Pos
type Layer a = Map Pos a
type Intel = Layer Int
data Cell = CMine
            | CDisarmed
            | CUnknown
            | CFree
       deriving (Eq)
type Field = Layer Cell

-- Returns (what-positions-to-probe, where-mines-are)
type Algorithm = Size -> Field -> Intel -> ([Pos], [Pos])

instance Show Cell where
  show CMine = "@"
  show CDisarmed = "*"
  show CUnknown = "#"
  show CFree = " "

-- It would be better to have this in multimap lib
instance (Show a, Show b) => Show (Mm.MultiMap a b) where
  show = show . Mm.toMap

layerSize :: Layer a -> Size
layerSize = M.foldlWithKey (\(mx, my) (x, y) _c -> (max mx x, max my y)) (0, 0)

neighbourDeltas :: [PosDelta]
neighbourDeltas = [(x, y) | x <- d, y <- d, x /= 0 || y /= 0]
  where d = [-1..1]

shiftPos :: Pos -> PosDelta -> Pos
shiftPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nearPositions :: Pos -> [Pos]
nearPositions pos = fmap (\d -> shiftPos pos d) neighbourDeltas

-- The cell is already known as not containing mine
isFreeCell :: Field -> Pos -> Bool
isFreeCell field pos = (M.lookup pos field) == (Just CFree)

filterLayer :: Layer a -> Field -> Layer a
filterLayer intel field = M.filterWithKey (\pos _ -> isFreeCell field pos) intel

genIntel :: Mines -> Intel
genIntel mines = foldl updateNeighbours M.empty (S.toList mines)
  where updateNeighbours intel pos = foldl (\intl nPos -> M.alter updateCount nPos intl)
                                           intel (nearPositions pos)
        updateCount Nothing = Just 1
        updateCount (Just x) = Just (x + 1)

genField :: Size -> Field
genField (sx, sy) = M.fromList [((x,y), CUnknown) | x <- [0..(sx-1)], y <- [0..(sy-1)]]

filterField :: Cell -> Field -> Set Pos
filterField cell field = M.keysSet $ M.filter (cell==) field

enumPositions :: Size -> [Pos]
enumPositions (width, height) = [(col, row) | col <- [0..(width-1)], row <- [0..(height-1)]]

inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = x >= lo && x < hi
