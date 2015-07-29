robo-minesweeper
================

Program that automatically plays Minesweeper game
(see https://en.wikipedia.org/wiki/Minesweeper_(video_game)).

In ambiguous situations algorithm attempts to minimize risk.

To build:
  cabal build

To run:
   dist/build/robominer/robominer
or specify rows, columns, mines count:
  dist/build/robominer/robominer 128 64 1200

In case of dense fields try:
  dist/build/robominer/robominer +RTS -N2 -K1g -RTS
(still may take forever to complete but at least it does not fail with exception)

TODO
----

* Implement remaining mines count heuristics (stepping randomly into unexplored area) 
* Cleanup/streamline code
* Profile to find out why positions enumeration takes very long time sometimes (in hope it can be optimized).
  
Hints
-----
  
Init sandbox:  
  cabal sandbox init  
  cabal install --only-dependencies   
  
