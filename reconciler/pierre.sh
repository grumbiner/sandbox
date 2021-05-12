#!/bin/sh
#resops = 1 for resops, 0 for psgrid, 2 for llgrid  Future: fix

FAMILY=llgrid
RESOPS=2

for SHOREFILE in gshhs_l.b gshhs_i.b gshhs_f.b
do
  for TARGET in pierre
  do
    #Need to ensure that new progs are produced:
    for x in bathy refill paving
    do
      if [ -f $x ] ; then
        rm $x
      fi
    done 
    export FAMILY RESOPS TARGET SHOREFILE
    time ./maskonly.sh  
  done
done

