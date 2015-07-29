{-# OPTIONS_GHC -Wall #-}
-- | Main loop
module Main where

import Common
import Algorithm
import Game
import qualified Board
import Board (BoardCell(..))

import qualified System.Random as SR
import qualified Control.Monad.Random as CMR

-- import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
-- import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Control.Monad as Cm
-- import Data.Sequence

import System.Console.ANSI

-- import qualified Control.Exception as CE
-- import System.IO
-- import qualified Control.Concurrent as CC
-- import Debug.Trace

genMine :: (SR.RandomGen g) => (Int, Int) -> CMR.Rand g (Int, Int)
genMine (maxX, maxY) = do
    x <- CMR.getRandomR (0, maxX - 1)
    y <- CMR.getRandomR (0, maxY - 1)
    return (x, y)

genMines :: Size -> Int -> IO Mines
genMines size cnt = do
      mines <- CMR.evalRandIO $ Cm.replicateM cnt $ genMine size
      return $ S.fromList mines

moveCursorBelow :: Size -> IO ()
moveCursorBelow boardSize = setCursorPosition (snd boardSize) 0

showStatus :: Size -> String -> IO ()
showStatus fieldSize message = do
    moveCursorBelow fieldSize
    putStrLn message

renderBoard :: Field -> Mines -> IO ()
renderBoard field mines = do
  setCursorPosition 0 0
  Board.renderIO (Board.cellsRender cellFn board)
  where
    cellFn _pos bc = (sgr, str)
      where
        sgr = case bc of
          Just (BCell CMine) ->     [SetConsoleIntensity BoldIntensity,
                                     SetColor Foreground Vivid Red]
          Just (BCell CDisarmed) -> [SetConsoleIntensity BoldIntensity,
                                     SetColor Foreground Vivid Green]
          _ ->                      [SetConsoleIntensity NormalIntensity,
                                     SetColor Foreground Dull White]
        str = case bc of
          Just x -> show x
          Nothing -> " "
    board = Board.fromBoards [
        Board.fromField field,
        Board.fromPositions (Board.BCell CMine) mines,
        Board.fromField $ M.filter (==CDisarmed) field]

showDebugInfo :: Size -> Mines -> Field -> IO ()
showDebugInfo fieldSize mines field =
  let intel = visibleIntel field mines
  in do
    moveCursorBelow fieldSize
    _ <- Cm.replicateM 3 $ putStrLn ""
    putStrLn "----Intel and mines----"
    putStrLn $ Board.showLayer $ Board.fromBoards [
      (Board.fromPositions (Board.BCell CMine) mines),
      (Board.fromIntel intel)]
    putStrLn "----Subtracted intel and disarmed ----"
    putStrLn $ Board.showLayer $ Board.fromBoards [
      (Board.fromField $ M.filter (==CDisarmed) field),
      (Board.fromIntel (subtractMines (filterField CDisarmed field) intel))]
    putStrLn "-->>"
    _ <-getChar
    return ()

render :: Size -> Mines -> Field -> IO ()
render fieldSize mines field = do
    -- clearScreen
    renderBoard field mines
    -- showDebugInfo fieldSize mines field
    return ()

step :: Size -> Mines -> Field -> IO ()
step fieldSize mines field =
  let (probePositions, newField) =
        gameStep fieldSize mines field choosePositions
      showFinalStatus message = do
        showStatus fieldSize message
        return ()
  in do
    -- CC.threadDelay 300000
    case newField of
      Nothing ->
        do render fieldSize mines field
           showFinalStatus $ "Tripped on mine at " ++ (show probePositions)
      Just f ->
        if isGameComplete mines f
          then do
                render fieldSize mines f
                showFinalStatus "Done"
          else if L.null probePositions
            then showFinalStatus "No probe position, disqualified."
            else do
                render fieldSize mines f
                step fieldSize mines f

main :: IO ()
main =
  let dims = (128, 64)
      field = (genField dims)
  in do
    mines <- genMines dims 1200 -- 1300 and more usually produces stack overflow exception
    clearScreen
    renderBoard field mines
    step dims mines field
