module Test where

import Common
import Algorithm
import qualified Data.Map as M
import qualified Data.Set as S

type DigestedTest = [(Pos, Char)]

digestField :: [String] -> DigestedTest
digestField strings = concat $ parseLines 0 strings
  where
    parseLines _row [] = []
    parseLines row (l:ls) =
      (zipWith (\c ch -> ((c, row), ch)) [0..] l) : parseLines (row + 1) ls -- Not TR

parseMines :: DigestedTest -> Mines
parseMines = S.fromList . (concatMap charToMine)
  where
    charToMine :: (Pos, Char) -> [Pos]
    charToMine (pos, ch) =
          case ch of
            '@' -> [pos]
            _ -> []

parseField :: DigestedTest -> Field
parseField = M.fromList . (concatMap charToCell)
  where
    charToCell :: (Pos, Char) -> [(Pos, Cell)]
    charToCell (pos, ch) =
          case ch of
            ' ' -> [(pos, CFree)]
            _ -> [(pos, CUnknown)]

parseIntel :: DigestedTest -> Intel
parseIntel = genIntel . parseMines

parseTest :: [String] -> (Mines, Field, Intel)
parseTest ls = (mines, field, filterLayer (genIntel mines) field)
  where
    digest = digestField ls
    mines = parseMines digest
    field = parseField digest

field0 :: [String]
field0 = ["   ###@",
          "   @@@#",
          "  @@###",
          "  @####",
          "#@ #@##",
          "#######"]

testData0 :: (Mines, Field, Intel)
testData0 = parseTest field0

--  (chooseProbePosition (7,6) 9 field intel)

-- > Actual: Tripped on mine at (1,4). Expected: Could be safely probing (3,0)

