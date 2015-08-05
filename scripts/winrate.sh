#!/bin/bash 

N=$1
W=$2
H=$3
M=$4

repeat () {
  for i in $(seq 1 $N);
  do 
    dist/build/robominer/robominer +RTS -N4 -K256m -RTS $W $H $M 
  done
}

# Outputs "density winrate"
bc <<< "scale=3; $M / ($W * $H)" | tr '\n' ' '
bc <<< "scale=3; $(repeat $N | grep Done | wc -l) / $N"
