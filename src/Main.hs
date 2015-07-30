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
import System.Environment (getArgs)

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

renderBoard :: Field -> Mines -> [Pos] -> IO ()
renderBoard field mines probes = do
  setCursorPosition 0 0
  Board.renderIO (Board.cellsRender cellFn board)
  where
    probeSet = S.fromList probes
    cellFn pos bc = (sgr, str)
      where
        sgr = case bc of
          _ | S.member pos probeSet ->
                                    [SetConsoleIntensity BoldIntensity,
                                     SetColor Foreground Vivid Red]
          Just (BCell CMine) ->     [SetConsoleIntensity BoldIntensity,
                                     SetColor Foreground Vivid Magenta]
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
    _ <- Cm.replicateM 8 $ putStrLn ""
    putStrLn "----Intel and mines----"
    putStrLn $ Board.showLayer $ Board.fromBoards [
      (Board.fromPositions (Board.BCell CMine) mines),
      (Board.fromIntel intel)]
    putStrLn "----Subtracted intel and disarmed ----"
    putStrLn $ Board.showLayer $ Board.fromBoards [
      (Board.fromField $ M.filter (==CDisarmed) field),
      (Board.fromIntel (subtractMines (filterField CDisarmed field) intel))]
    putStrLn "-->>"
--     _ <-getChar
    return ()

render :: Size -> Mines -> [Pos] -> Field -> IO ()
render fieldSize mines probes field = do
--    clearScreen
    renderBoard field mines probes
--     showDebugInfo fieldSize mines field
    return ()

step :: Size -> Mines -> Field -> Algorithm -> IO ()
step fieldSize mines field algorithm =
  let (probePositions, newField) = gameStep fieldSize mines field algorithm
      showFinalStatus message = showStatus fieldSize message
      render0 = render fieldSize mines probePositions
  in do
    -- CC.threadDelay 300000
    case newField of
      Nothing ->
        do render0 field
           showFinalStatus $ "Tripped on mine at " ++ (show probePositions)
      Just f ->
        if isGameComplete mines f
          then do
                render0 f
                showFinalStatus "Done"
          else if L.null probePositions
            then showFinalStatus "No probe position, disqualified."
            else do
                render0 f
                step fieldSize mines f algorithm

run :: Size -> Int -> Algorithm -> IO ()
run fieldSize mineCount algorithm = do
    putStrLn $ "size=" ++ show fieldSize
    putStrLn $ "mines=" ++ show mineCount
    mines <- genMines fieldSize mineCount -- 1300 and more usually produces stack overflow exception
    clearScreen
    let field = genField fieldSize
    renderBoard field mines []
    step fieldSize mines field algorithm

data AlgorithmSwitch = Default | Fancy
 deriving (Read)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cols, rows, minesCount, alg] ->
      runWithArgs cols rows minesCount
        (case (read alg) of
           Fancy -> choosePositions2
           _ -> defaultAlgorithm)
    [cols, rows, minesCount] ->
      runWithArgs cols rows minesCount defaultAlgorithm
    _ ->
      run (100, 40) 600 defaultAlgorithm
  where
    runWithArgs cols rows minesCount = run (read cols, read rows) (read minesCount)
    defaultAlgorithm = choosePositions