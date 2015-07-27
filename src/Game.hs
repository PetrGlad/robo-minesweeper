module Game where

import Common

import Data.Map (Map)
import qualified Data.Map as M
import qualified Control.Monad as Cm

import qualified Data.Set as S

probeStep :: Mines -> Field -> Pos -> Maybe Field
probeStep mines field pos
  | S.member pos mines = Nothing
  | otherwise = Just $ newField
  where newField :: Map Pos Cell
        newField = M.alter (const (Just CFree)) pos field

-- Return probe positions and new field
gameStep :: Size -> Mines -> Field -> Algorithm -> ([Pos], Maybe Field)
gameStep fieldSize mines field algorithm = (probePositions,
                                            Cm.foldM (probeStep mines) field probePositions)
  where
    -- Constraint: set of mines should not be available to algorithm (chooseProbePositions)
    intel = filterLayer (genIntel mines) field
    probePositions = fst $ algorithm fieldSize field intel


