module Board where

import Common

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set (Set)

import System.Console.ANSI

data BoardCell = BCell Cell
                 | BIntel Int
type Board = Layer BoardCell

empty :: Board
empty = M.empty

compose :: Board -> Board -> Board
compose x y = M.union y x

make :: (a -> BoardCell) -> Layer a -> Board
make f l = M.map f l

instance Show BoardCell where
  show (BCell c) = show c
  show (BIntel n) = show n

-- Useful for refgression testing / debugging
showLayer :: Show a => Layer a -> String
showLayer layer = unlines $ map mapRow [0..(snd sz)]
  where
    sz = layerSize layer
    mapRow r = concatMap (getCell r) [0..(fst sz)]
    getCell r c =
      case M.lookup (c, r) layer of
        Nothing -> " "
        Just v -> show v

fromField :: Field -> Board
fromField = M.map BCell

fromPositions :: BoardCell -> Set Pos -> Board
fromPositions v = M.fromSet (const v)

fromIntel :: Intel -> Board
fromIntel = M.map BIntel

fromBoards :: [Board] -> Board
fromBoards = foldl Board.compose Board.empty

-- -- Fancy render board to ANSI console
-- ansiRender :: (Pos -> BoardCell -> ([SGR], String)) -> Board -> IO ()
-- ansiRender cellFn board = mapM mapRow [0..(snd sz)]
--   where
--     sz = layerSize layer
--     mapRow r = concatMap (getCell r) [0..(fst sz)]
--     cellAction () c =  r c =
--     case M.lookup (c, r) layer of
--       Nothing -> " "
--       Just v -> show v