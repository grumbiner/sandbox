#!/bin/sh

#Score persistence as a forecaster
#Robert Grumbine

base=`pwd`
for level in 0.50 0.70 0.90 0.95
do
  for d in 8 5 45 32 3 24 16 12 10 1 365
  do
    cd $base/$d
    grep "level $level " *.198[2-9]* *.199* > ../l${level}d$d 
    grep "level $level " *.2* >> ../l${level}d$d 

    cd $base
    ./a.out l${level}d$d > l${level}d${d}.s
  done
done
    

