robo-minesweeper
================

Program that automatically plays Minesweeper game
(see https://en.wikipedia.org/wiki/Minesweeper_(video_game)).

In ambiguous situations algorithm attempts to minimize risk.
The algorithm:
 1. Enumerate all mine layouts that are consistent with intel. 
 2. Finding probabilities of a mine in an unknown position. 
 3. Then probe positions with least probability of mine in it.

To build:
  cabal build

To run:
   dist/build/robominer/robominer
or specify rows, columns, mines count:
  dist/build/robominer/robominer 128 64 1200
In case of dense fields try:
  dist/build/robominer/robominer +RTS -N2 -K1g -RTS
(still may take forever to complete but at least it does not fail with exception)

Add "Fancy" to run with alternative algorithm
  dist/build/robominer/robominer 128 64 1000 Fancy

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
  
