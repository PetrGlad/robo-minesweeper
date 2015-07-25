robo-minesweeper
================

Program that automatically plays Minesweeper game
(see https://en.wikipedia.org/wiki/Minesweeper_(video_game)).

In ambiguous situations algorithm attempts to minimize risk.

TODO
====

* Cleanup/streamline code
* Write tests, cover edge cases 
* Implement remaining mines count heuristics (stepping randomly into unexplored area) 
* Optimize: Mark mines that are already found and cleanup intel to reduce dependencies
  (now it produces stack overflow on (128, 64) with 1500 mines)
  
Hints
=====
  
Init sandbox:  
  cabal sandbox init  
  cabal install --only-dependencies   
  
