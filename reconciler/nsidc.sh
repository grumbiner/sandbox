#!/bin/sh
#resops = 1 for resops, 0 for psgrid, 2 for llgrid  Future: fix

FAMILY=psgrid
RESOPS=0

for TARGET in nsidcnorth nsidcsouth 
do
  for x in bathy refill paving
  do
    if [ -f $x ] ; then
      rm $x
    fi
  done 
  for SHOREFILE in gshhs_c.b gshhs_l.b gshhs_i.b 
  do
    export FAMILY RESOPS TARGET SHOREFILE
    time ./new_build.sh  
    #Need to ensure that new progs are produced:
  done
done

#Separate the fine resolution coastline's run due to the large number 
#   of points in the file
for TARGET in nsidcnorth nsidcsouth 
do
  for x in bathy refill paving
  do
    if [ -f $x ] ; then
      rm $x
    fi
  done 
  for SHOREFILE in gshhs_f.b
  do
    export FAMILY RESOPS TARGET SHOREFILE
    time ./new_build.sh  
    #Need to ensure that new progs are produced:
  done
done

