module Game where

import Common

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Control.Monad as Cm

import qualified Data.Set as S

probeStep :: Places -> Field -> Pos -> Maybe Field
probeStep mines field pos
  | S.member pos mines = Nothing
  | otherwise = Just $ newField
  where newField :: Map Pos Cell
        newField = M.alter (const (Just CFree)) pos field

-- Return probe positions and new field
gameStep :: Size -> Places -> Field -> Algorithm -> ([Pos], Maybe Field)
gameStep fieldSize mines field algorithm = (probePositions,
                                            Cm.foldM (probeStep mines) field probePositions
                                            >>= disarmMines)
  where
    -- (!) Constraint: set of mines should not be available to algorithm (chooseProbePositions)
    (probePositions, foundMines) = algorithm fieldSize field (visibleIntel field mines)
    disarmMines fld = Just $ L.foldl (\f m -> M.alter (\_ -> Just CDisarmed) m f) fld foundMines

isGameComplete :: Places -> Field -> Bool
isGameComplete mines field =
  (mines == (M.keysSet $ M.filter (CDisarmed==) field))
  && (M.null $ M.filter (CUnknown==) field)

visibleIntel :: Field -> Places -> Intel
visibleIntel field mines = filterLayer (genIntel mines) field