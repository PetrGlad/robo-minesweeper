module Board where

import Common

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
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

renderColored :: [SGR] -> IO () -> IO ()
renderColored colorSetup action = do
  setSGR colorSetup
  action
  setSGR [Reset]

type CellRender = ([SGR], String)

-- ------------------------------------
-- Rows based renderer

-- Fancy render board for ANSI console
cellsRender :: (Pos -> Maybe BoardCell -> CellRender) -> Board -> [[CellRender]]
cellsRender cellFn layer = map mapRow [0..(snd sz)]
  where
    sz = layerSize layer
    mapRow r = map (getCell r) [0..(fst sz)]
    getCell row col = cellFn (col, row) (M.lookup (col, row) layer)

-- Send rendered board to ANSI console
renderIO :: [[CellRender]] -> IO ()
renderIO cells = mapM renderRow cells
                 >> setSGR [Reset]
  where
    renderRow r = mapM renderChar r
                  >> putStrLn ""
    renderChar (color, str) = do
      setSGR color
      putStr str

-- ------------------------------------
-- Positions based renderer

renderCell :: Show a => Pos -> a -> IO ()
renderCell (x, y) c = do
               setCursorPosition y x
               putStr (show c)

-- Set-cursor-position render to ANSI console
renderLayer :: Show a => Map Pos a -> IO ()
renderLayer cells = mapM_ (uncurry renderCell) (M.toList cells)
