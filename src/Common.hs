{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Declarations that are common for Main and Algorithm modules
module Common where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.MultiMap as Mm
import Data.Set (Set)

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

instance Show Cell where
  show CMine = "@"
  show CUnknown = "#"
  show CFree = " "

-- It would be better to have this in multimap lib
instance (Show a, Show b) => Show (Mm.MultiMap a b) where
  show = show . Mm.toMap

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
