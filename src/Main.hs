{-# OPTIONS_GHC -Wall #-}
-- | Main loop and rendering
module Main where

import Common
import Algorithm

import qualified System.Random as SR
import qualified Control.Monad.Random as CMR

import Data.Map (Map)
import qualified Data.Map as M
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
      mines <- CMR.evalRandIO $ Cm.replicateM cnt  $ genMine size
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

isGameComplete :: Mines -> Field -> Bool
isGameComplete mines field = S.size mines == L.length (filter (CUnknown==) (M.elems field))

step :: Mines -> Field -> Pos -> Maybe Field
step mines field pos
  | S.member pos mines = Nothing
  | otherwise = Just $ newField
  where newField :: Map Pos Cell
        newField = M.alter (const (Just CFree)) pos field

genIntel :: Mines -> Intel
genIntel mines = foldl updateNeighbours M.empty (S.toList mines)
  where updateNeighbours intel pos = foldl (\intl nPos -> M.alter updateCount nPos intl)
                                           intel (nearPositions pos)
        updateCount Nothing = Just 1
        updateCount (Just x) = Just (x + 1)

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
      probePoss = (chooseProbePosition fieldSize minesCount field intel)
      newField = Cm.foldM (step mines) field probePoss

      showFinalStatus message = do
        showStatus fieldSize message
        return ()
  in do
    clearScreen
    renderBoard field intel mines
    moveCursorBelow fieldSize -- Move it away so it does not obstruct cells

    -- CC.threadDelay 300000
    -- _ <- getChar

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
  let dims = (64, 32)
  in do
    clearScreen
    mines <- genMines dims 350
    gameStep dims mines (genField dims)
