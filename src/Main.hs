{-# OPTIONS_GHC -Wall #-}
-- | Main loop and rendering
module Main where

import Common
import Algorithm
import Game
import qualified Board

import qualified System.Random as SR
import qualified Control.Monad.Random as CMR

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
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

moveCursorBelow :: Size -> IO ()
moveCursorBelow boardSize = setCursorPosition (snd boardSize) 0

showStatus :: Size -> String -> IO ()
showStatus fieldSize message = do
    moveCursorBelow fieldSize
    putStrLn message

renderBoard :: Field -> Mines -> IO ()
renderBoard field mines =
  let minesColor = [SetConsoleIntensity BoldIntensity,
                    SetColor Foreground Vivid Red]
      fieldColor = [SetColor Foreground Dull White]
  in do
    renderColored fieldColor $ renderLayer field
    renderColored minesColor $ renderLayer $ positionsToLayer (const CMine) mines
    renderColored fieldColor $ renderLayer (M.filter (==CDisarmed) field)

render :: Size -> Mines -> Field -> IO ()
render fieldSize mines field =
  let newIntel = visibleIntel field mines
  in do
    -- clearScreen
    renderBoard field mines
    renderLayer newIntel
    moveCursorBelow fieldSize -- Move it away so it does not obstruct cells
--     Cm.replicateM 3 $ putStrLn ""
--     putStrLn "------mines-----"
--     putStrLn $ Board.showLayer $ Board.fromBoards [
--       (Board.fromPositions (Board.BCell CMine) mines),
--       (Board.fromIntel newIntel)]
--     putStrLn "------Subtracted----"
--     putStrLn $ Board.showLayer $ Board.fromBoards [
--       (Board.fromField $ M.filter (==CDisarmed) field),
--       (Board.fromIntel (subtractMines (filterField CDisarmed field) newIntel))]
--     putStrLn "-->>"
--     c <-getChar
    return ()

step :: Size -> Mines -> Field -> IO ()
step fieldSize mines field =
  let (probePositions, newField) =
        gameStep fieldSize mines field choosePositions
      showFinalStatus message = do
        showStatus fieldSize message
        return ()
      intel = visibleIntel field mines -- Debug
  in do
    -- CC.threadDelay 300000
    -- _ <- getChar
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
    mines <- genMines dims 1000
    clearScreen
    renderBoard field mines
    step dims mines field
