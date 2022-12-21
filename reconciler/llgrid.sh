#!/bin/sh
#resops = 1 for resops, 0 for psgrid, 2 for llgrid  Future: fix
#Robert Grumbine 14 Aug 2008

FAMILY=llgrid
RESOPS=2

#for SHOREFILE in try50.b try25.b try15.b try8.b try4.b try2.b
for SHOREFILE in gshhs_c.b gshhs_l.b gshhs_i.b
do
  #for TARGET in global_12th
  for TARGET in global_sst global_ice global_quarter 
  do
    #Need to ensure that new progs are produced:
    for x in bathy refill paving
    do
      if [ -f $x ] ; then
        rm $x
      fi
    done 
    export FAMILY RESOPS TARGET SHOREFILE
    time ./new_build.sh  
  done
done

