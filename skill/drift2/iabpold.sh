#!/bin/bash
set -xe

#Loop through for different ranges 
#for RADIUS in 55.0 110.0 165.0 
for RADIUS in 55.0 
do
  if [ ! -d km${RADIUS} ] ; then
    mkdir km${RADIUS}
  fi
  for flag in 98 97 95 94 93
  do
    ln -f C$flag.iabp dboydata
    time ./iabpcheck $RADIUS 3.0 16 checked.$flag > comments.$flag.$RADIUS
    time ./avg2 $RADIUS 3.0 16 checked.$flag fout1.$flag fout2.$flag > avg.$flag.$RADIUS
    mv checked.$flag comments.$flag.$RADIUS fout1.$flag fout2.$flag km${RADIUS}
  done
done
#Somthing is wrong with 1996 file
