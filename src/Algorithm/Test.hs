module Test where

import Common
import Algorithm
import Game
import qualified Board

-- Large, Medium, Small
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Test.QuickCheck

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

-- Parses board representation that is copy-pasted from console and split into [String]
-- One can post-process result or pick parts that are needed.
parseTestData :: [String] -> (Size, Mines, Field, Intel)
parseTestData ls = ((cols, rows), mines, field, filterLayer (genIntel mines) field)
  where
    digest = digestField ls
    mines = parseMines digest
    field = parseField digest
    allPositions = map fst digest
    mx = L.foldl max 0
    cols = 1 + (mx (map fst allPositions))
    rows = 1 + (mx (map snd allPositions))

data GameResult =
  Done
  | Tripped [Pos]
  | Disqualified
  deriving (Show, Eq)

-- Game step that does not do IO (in contrast to Main.step)
headlessStep :: Size -> Places -> Field -> (Field, GameResult)
headlessStep fieldSize mines field =
  let (probePositions, newField) = gameStep fieldSize mines field choosePositions
  in case newField of
      Nothing -> (field, Tripped probePositions)
      Just f ->
        if isGameComplete mines f
          then (f, Done)
          else case probePositions of
            [] -> (f, Disqualified)
            _ -> headlessStep fieldSize mines f

-- Regression tests

field0 :: [String]
field0 = ["   ###@",
          "   @@@#",
          "  @@###",
          "  @####",
          "#@ #@##",
          "#######"]

testData0 :: ([Pos],[Pos])
-- > Actual: Tripped on mine at (1,4).
testData0 = choosePositions (7,6) field intel
  where (size, mines, field, intel) = parseTestData field0

runSavedGame :: [String] -> (Field, GameResult)
runSavedGame fieldData = headlessStep size mines field -- (genField size)
  where (size, mines, field, intel) = parseTestData fieldData

reRunSavedGame :: [String] -> (Field, GameResult)
reRunSavedGame fieldData = headlessStep size ms (genField size)
  where
    (size, mines, field, intel) = parseTestData fieldData
    ms = S.union mines $ filterField CDisarmed field


-- Helper for use in GHCI
tryTest :: (Field, GameResult) -> IO ()
tryTest tst = do
   let (f, r) = tst
   print r
   putStrLn $ Board.showLayer f


test1 :: (Field, GameResult)
test1 = runSavedGame field1

-- Stack space overflow: current size 8388608 bytes.
field1 = [
    "   111  1#@1       11111212@1 1@11@1111  1@21  2@@2122@1    1@1",
    "   1@1123@#21111 123@22@2@211 1111111@1  12@2124@53@3@4321  111",
    "   1111@@22@11@1 1@@22@2211 111111  111 11213@4@@4@23@3@@211",
    " 111  233111111112321222111 1@11@21  1233@1 3@@4@311112222@321",
    "12@1  1@112321  2@2  1@11@2122323@1  1@@@2213@3211    11112@@211",
    "@322  1111@@@1  2@31111234@11@2@211  123211@22321     1@1 1222@1",
    "23@22222122321 1233@1 13@@3111211         1112@@1     222 111122",
    "1@4@3@@4@1  1111@2@3211@@@2  111 111  111    2@31     1@1 1@1 1@",
    "12@3@4@@431 1@111324@223421  1@211@1  1@1 111111      111 223121",
    " 2342223@@1 111123@3@21@1 11112@11222121212@1  111  111 123@3@1",
    " 1@@2222332111 2@@3422111 1@1 111 1@2@1 1@211  2@3111@1 1@@5@31",
    "12222@@11@11@1 3@6@3@1111 1221 11111322 12211112@4@21221134@@211",
    "@1  2331112221 3@6@3111@2123@2 2@2  1@2111@11@1114@3 1@1 1@4311@",
    "11  1@1   1@1113@@2111334@3@@213@2  112@1111111  2@31112122@2132",
    "    11211 1111@2221 1@3@@34@322@21    111     11112@2122@2212@3@",
    "11    1@1111 111    124@32@211@21             1@21113@3@3@1 114@",
    "@1 1111122@1  111    1@33321 2331         122112@3323@313331  2@",
    "11 1@2111@21  1@21   112@@3211@@21111     1@@2122@@@321 1@@11121",
    "   113@3221  134@21 111123@@1134@11@1     1333@11234@2  12322@1",
    " 12212@3@211 1@@3@1 1@1 12432 1@32111111 123@2111112@2    2@3121",
    " 2@@111212@1 122322 111 1@2@1 12@21  1@213@@321 2@21221   2@2 1@",
    " 3@41    111 1112@31 111112221 12@1  112@3@32@1 2@213@2   111 11",
    " 2@2  111    1@13@@1 1@1 123@211221    11211111 1111@@31 111  11",
    " 223112@2111 1112@31 111 1@@3@11@1                1244@1 2@2112@",
    " 1@2@12@21@1   12321  122222211111           111  1@2@21 2@21@43",
    " 112122211221  1@2@21 1@@21    111    11211112@21 123332111113@@",
    "11   1@1  1@21 1122@1 134@1    1@222112@3@11@23@2  1@2@@1 1112@3",
    "@11111221123@211  111  1@21111 113@@11@4@433212@2  112221 1@1122",
    "221@222@22@212@1 111   11112@1   2@31113@3@@1 111         11112@",
    "@222@2@22@21 222 1@1 111  1@21   111   11344311     111 112111@2",
    "2@2212111221 1@21211 1@222211 111        1@@4@2     1@1 1@3@2222",
    "12@1     1@1 112@1   112@@1   1@1        123@@2     111 113@21@1"]


-- Stack space overflow: current size 8388608 bytes.
test2 = runSavedGame [
    " 1@@2   123211#@@@#####@###############@#@##@###########@#######",
    "123@2   2@@@32@##@##@#@##########@###@##@#########@#####@####@##",
    "@1111   2@6@#@3##@###@######@##@###@####@@#####@@###@@#####@###@",
    "11 111  113@#@##@#@####@################@###@##@@#####@#@#######",
    "1112@1111 23@##############@@#####@####@#@#@@@##@##############@",
    "@12@422@1 1@4@@################@#########@#########@#@@##@#@####",
    "333@3@211 235@######@#######@##############@@##@#@@##@#######@#@",
    "@@21211   1@@############@@#@#############@##@########@#########",
    "221       14@@###@##@############@###########@#######@#########@",
    "           2@4######@@#####@@###@#########@####@##@###@##@#@####",
    "11         112@########@################@##@#######@#####@#@####",
    "@1  1221     1###@#@###################@###@@#######@@#######@##",
    "221 1@@2   112####@@@############@@@#################@########@#",
    "1@1 24@31  2@3@########@########@#####@#@###############@#######",
    "221 1@3@21 3@##########@############@########@#################@",
    "@1112122@1 2@@###@#######@@########@######@################@####",
    "111@1 1332 124@######@########@###############@#@######@########",
    "1122212@@1   2@#@#@###@###@######@#######@##@#@################@",
    "#@2#@1#@52  12########@####@###############@####@@##@###########",
    "#@#####@@####@######@#######@##@#########################@####@#",
    "#####@##@##@#######################@#####@#############@########",
    "#@#@#@#@##@##@#@###@###########@#@#######@##@#@####@####@#######",
    "########@########@######@##################################@###@",
    "###@#######@########@@#######@######@##@##@#######@####@#######@",
    "##########@###########@##@#########@#######@@#@#####@########@##",
    "########@###################@##########@###@############@@######",
    "@#####@###################@@####@@######@#@###########@######@##",
    "#@####@########@#############@#@######################@######@##",
    "###@######@#@##@#@#######@#####@@###########@#########@##@##@###",
    "##@#########@@#########@#@#@#@####@###@###@#####@@####@#####@###",
    "##########@################@####@####################@##@#######",
    "@@@##########@@###############@@#@@##@####@####@#@#@####@#######"]

-- This one takes minutes to complete in ghci and causes SOE in compliled binary
-- GameResult == Tripped [(0,23)]
test3 = runSavedGame [
    " 2@21@1             1@@2     1@11######@#@####@######@#####@####",
    " 2@2111    111      13@2  1111233@#@@###@#######@#####@#@#@#@###",
    " 111       2@2   11212221 1@1 1@@3@3######@@###########@########",
    " 111111 1112@21112@3@22@1 111 122211#######@#############@######",
    " 1@22@112@23332@24@412@21111        ###@##@###@#@###@##@#######@",
    " 12@2222@3@2@@32@3@31111 1@1     111####@##@#@@##############@@#",
    " 13332@323123@22232@1111 111     1@1###############@############",
    "13@@2@33@1  1111@22212@2      111111#@###@#####@###@@#####@@#@##",
    "1@@3212@21111  123@1 2@2      1@1111#@@#@#@###@#@@#######@@#####",
    "2331  12223@2   1@21 111      1111@2###############@##@#####@@#@",
    "1@1   12@3@@4321223111221        223@######@#@@########@@##@###@",
    "222   1@3@#4@@@11@2@11@@1        1@2####@####@####@######@@#####",
    "2@312122###@5@31112111221   11211112####@#####@#@####@########@#",
    "2@3@2@22@###@31111       1111@3@1112@#######@@###@##@########@##",
    "3331212@2###@2 1@211221  1@223@211@3###@@##@#####@@@###@######@#",
    "@@1   112##122124@21@@111323@211 12@####################@#######",
    "332 11113@# 1@11@@212322@4@3221  12##################@##########",
    "1@211@23@@#12111221 12@34@@32@1  1@###################@#########",
    "##@####@@##@1  112222@4@#@4@#22111###############@##############",
    "###@@#####@21112@2@@33@#####@##@#########@##############@#######",
    "########@#####@######@#######@#@@@@#########@###########@#######",
    "##@@#####@###@########@#######################@#############@@#@",
    "#@########@@#################@##@##@#####@@@#@##############@###",
    "@######@###@###################@#######################@########",
    "###@@#@####@########@####@#@####@##########@####@#@####@#####@##",
    "#####@################@#############@####@#####@##@#####@#######",
    "######@############@@#@@####@#######@####@#@###############@####",
    "####@######@#@###@###@#######@##@@@##@#@#@#########@#@##########",
    "@@#######@###########@#############@#@####@#@#######@###########",
    "@###########@########@############@########@###########@##@##@#@",
    "####@@#@###@####@@#####@#@##@################@@#########@#######",
    "#######@#####@######@#####@###########@#@##@#######@#######@@@##"]

-- This one should pass cleanly
test4 = runSavedGame [
    "  1111@1 113@21111##############@#@######@#########@#######@##@#",
    "111@2221 2@6@31@22@@@###@###@##@#@@#############################",
    "@2222@1  3@@@212#@#######@###@###@####@@@@######@###############",
    "12@1111  3@63112@##@#1  1#######@##########@########@####@#@#@#@",
    "23321 1124@@211@4@4#@21 1################@######################",
    "@@2@211@2@@5@2334@@22@1 1@#@####@###################@##@#######@",
    "@323@21123@312@@2221111 1#######@####@####@###############@#####",
    "11 2@211111113431        ##########@####################@######@",
    "   1111@211 1@@211  111  #########@##########@##########@#######",
    " 111  112@1 2332@1  1@1 1##@##@#########@######################@",
    " 1@21 1132212@11111122212@#####@#############################@##",
    "122@211@3@11@332112@11@24@#####@######@####@#@##########@@##@###",
    "@212@123@32212@@22@2113@4@##############@#######################",
    "@311111@22@1 1333@21  2@3#######@##@############@##@####@#######",
    "3@31  233212111@211   111#########@###@##@#####@######@@########",
    "2@@1  1@@1 1@111211      ######@#########@#################@####",
    "1221 134321212123@311    ######@@####@@#####@##@#@@###@#####@##@",
    "    12@@11@1 1@2@@3@21111##@####@#@@###@#@@@################@###",
    " 1111@32223222223454@#2@##############@@########################",
    " 1@112322@3@3@211@@@#@##############@@#######@########@@#####@##",
    " 111 1@@213@33@3233########@#########@###@@####@################",
    "11112443112212@3@22##@###@#####@#####@#########@##@#@###@#@#@##@",
    "1@11@@@1 2@2 1122@#@#@#####@####@######################@####@@##",
    "11112321 2@2    2############@#######@###@###@###@##@######@####",
    " 111111  111    2@#@######@###########@########@#@#####@#@###@@#",
    " 1@11@21   111113@##@#########@##@@#@#########################@#",
    " 22212@1   1@23@4#######@#@########@#@#@@##@#@@###@@#@@##@@###@#",
    " 1@1 111 1133@#@@#@########@#@##@#########@#####@###@#@#########",
    "1221     2@4@#############@@####@####@#########@#####@#######@##",
    "1@2221 124@@#@@################@######@@#######@######@#########",
    "112@@1 1@@4#######@#@###@#########@############@################",
    "##########@###@##############@#@##@##@###@###@####@#############"]

-- This one seems to take forever (but there are sure probes to take)
test5 = runSavedGame [
    "1222@1    1@2@23@21@11@1    1@12@31 2@2 1@2@1 111111   #########",
    "1@@211112121212@@211111212111112@@312@2 11211 1@11@1   ####@####",
    "24531 1@2@11111221     1@3@1   13@@22331  1221122211   ###@#####",
    "1@@@21112123@2         12@21    13@21@@21 1@@212@1 1111##@####@#",
    "1244@1    1@@2 111 111 1221     1333233@211222@211 2@21@########",
    " 12@21    1332 1@1 2@322@1      1@@2@123@111223321 2@21#########",
    " 2@31      1@1 111 2@@2@21 1221 1222111@322@4@2@@212221########@",
    " 2@31111  1332     122211113@@1  111  12@12@@223@33@32@#########",
    " 23@11@1  1@@1  111      2@4@421 1@1 112111332 23@4@@#2#@#######",
    " 1@22221 12321112@1 112223@312@1 111 1@211 1@1 1@3@###@#####@###",
    " 2232@1112@1 12@21213@3@@2121211     112@1 111 112##@#####@#####",
    " 1@2@211@22211@21 1@3@323211@1     111 111       12@####@#######",
    " 1122222333@1111  11211 1@2211     1@321   111  12@############@",
    "    1@2@3@@21     111   12@1       12@@21111@3222@#@#########@##",
    "  1132313@31111   1@21112221   1121124@32@112@@2@3############@#",
    "  2@3@21322 1@1  123@22@2@21   1@3@32@3@223223343#@##########@##",
    "112@423@3@22321  1@212@233@1   113@4@32111@@11@3@@##@##########@",
    "@2112@22@22@@2  1332 1123@31   11213@311 133223@4#####@#####@#@#",
    "@31 11111224@4211@@1   1@@3221 1@11222@1 12@22@4@##@#######@####",
    "3@21     1@33@@222321  1222@@1 1111@112212@4@23@5@#221@###@###@@",
    "@4@211   112@33@1 1@321   2342211 11112@23@4222@@22@111####@####",
    "3@43@21   1332111 12@@2 112@2@3@2    1@3@223@223211111####@###@#",
    "2@3@3@1   1@@1    124@311@32313@2    112111@22@1    12@#########",
    "1232211   1332    1@22@1112@1 111  1221   112221  112@2#########",
    " 1@1       1@1    11122211211 111 12@@1  1111@1   1@212#@#######",
    " 111111    111     112@22@1 112@1 1@432111@1222   11112@########",
    "22212@321      111 1@22@33222@322 12@22@12221@1      1@#########",
    "@@3@33@@1 111  1@1 11223@2@2@22@211223@211@1222 1122333@########",
    "###@##@3112@1  11211 1@2133421112@11@22333211@212@2@@@2#######@#",
    "##@####211@21    1@1 111 1@@22211111122@@@1 113@3234######@###@#",
    "##@##@3@3211    1221     1222@@1     1@4@321114@31@3@##@#######@",
    "##@####@@1      1@1         1221     112#11@11@@212@2#@#########"]

-- Tripped [(0,29)]
test6 = runSavedGame [
    "    @     @                   @#@ ####@#######@@#@#@@@######@###",
    "           @@ @ @ @         @ @   @####@########@###########@###",
    "    @  @@ @   @  @         @     @#####@##@###@#@######@##@@####",
    "    @        @            @    @  ######################@#@#@@@#",
    " @   @    @                    @  ####@@#######@#@#####@#@######",
    "         @    @  @  @ @          @#############@####@#########@#",
    "  @     @  @       @             @@####@###@##@#@########@######",
    "  @   @ @ @                       @########@####@@##############",
    "            @   @              @  #########@####@#@@##@#@#######",
    "               @  @   @  @@       ##@@########@#########@####@##",
    " @ @@@@ @                         ####@#@###@@#####@@##@########",
    "@  @  @  @        @ @@            #@#########@####@@###@#####@##",
    "        @ @  @  @@          @@    @####@######@#################",
    "#    @  @   @ @             @##@######@##@#######@####@###@#####",
    "#    @    @            @   @ #@##@###@#####################@##@#",
    "#        @    @      @ @    #@#@#@##@#@@#@##@@#@################",
    "##    @@@     @  @          @  @@ ###@###@##########@####@######",
    "@#         @       @    @         @###@###############@#####@@@#",
    "##    @ @    @@          @  #     #############@@##########@####",
    "#####          @   @        @    @#@###@########@#########@#####",
    "#####   @  @    @     @  @ @  @@  ##################@@##########",
    "##@##                     #@########@#######@######@#####@######",
    "##@@#  @      @    @ @    @####################@@###############",
    "#@###@#    @@      @       @######@#########################@##@",
    "##@#@@@##@##             @##@############@@@##@#@#@########@@###",
    "######@##### @ @ @   @@   ####################@##@###########@##",
    "#####@##@#@##@####@########@@##@#########@#######@######@###@@@#",
    "#@##########################@#####################@#############",
    "################@####@########@#####@#####@#####@#######@######@",
    "@@###@####@@####@###########################@####@###########@@#",
    "#@###########@##@#######@######@###@########@#######@#####@#####",
    "###@###@##@##@#####################@@################@#####@##@#"]

-- This one should pass cleanly
test7 = reRunSavedGame [
    "      @###     @#@##############",
    "   *     @      @##@#######@####",
    "                @####@##########",
    " *              @@@####@########",
    "                ############@###",
    "              @##########@######",
    "   @         @####@@############",
    "   @#@   @   ###################",
    "   ###       ######@#####@##@###",
    "@#####      @###@######@########",
    "######@     @#####@#######@#####",
    "#######@     ########@##########",
    "#####@@#     @##############@###",
    "########      ##################",
    "#########@    ##################",
    "#####@##@#    @###@#############"]

-- This one should pass cleanly
test8 = reRunSavedGame [
    "##############",
    "###@##########",
    "###@@@@###@###",
    "###@###@@@@###",
    "###@@#@######",
    "#####@########"]

-- This one should pass cleanly
test9 = reRunSavedGame [
    "#########",
    "#########",
    "@@@@@@@@@",
    "#########",
    "#########"]

-- This one should pass cleanly
test10 = reRunSavedGame [
    "####@####",
    "####@####",
    "####@####",
    "####@####",
    "####@####"]
