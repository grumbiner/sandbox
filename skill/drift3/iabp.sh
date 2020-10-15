#!/bin/bash
#Check through iabp files for buoys which are near to skiles points,
#  where 'near' is defined in the for loop.

set -x

ln -s /usr1/rmg3/drift/fix/forecast.points forecast.points

#Loop through for different ranges 
flag=full
for RADIUS in 20.0 27.5 40.0 55.0 80.0 110.0 165.0 
do
  if [ ! -d km${RADIUS} ] ; then
    mkdir km${RADIUS}
  fi
  if [ ! -f dboydata ] ; then
    ln -sf /usr1/data2/2d/buoy/iabp/full.iabp dboydata
  fi
  time ./iabpcheck $RADIUS 3.0 16 checked.$flag.$RADIUS > comments.$flag.$RADIUS
  mv checked.$flag.$RADIUS comments.$flag.$RADIUS fout1.$flag.$RADIUS fout2.$flag.$RADIUS km${RADIUS}

done
