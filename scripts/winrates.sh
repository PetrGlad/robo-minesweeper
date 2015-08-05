#!/bin/bash

for i in $(seq 10 20 360);
do
  ./winrate.sh 20 40 40 $i
done
