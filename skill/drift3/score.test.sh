#!/bin/bash
#Script to split out drift verification figures for sea ice model scoring

set -x

#Loop through for different ranges 
flag=full
for RADIUS in 55.0 27.5 110.0 77.5 38.75 
do
  if [ ! -d km${RADIUS} ] ; then
    mkdir km${RADIUS}
  fi
  if [ ! -f dboydata ] ; then
    ln -f iabp.scoring dboydata
  fi
  time ./iabpcheck $RADIUS 3.0 16 checked.$flag.$RADIUS > comments.$flag.$RADIUS
  time ./avg3 $RADIUS 3.0 16 checked.$flag.$RADIUS fout1.$flag.$RADIUS fout2.$flag.$RADIUS > avg.$flag.$RADIUS

done

exit

  time ./avg2 $RADIUS 3.0 16 checked.$flag.$RADIUS fout1.$flag.$RADIUS fout2.$flag.$RADIUS > avg.$flag.$RADIUS
  ./splityear.pl < fout2.$flag.$RADIUS
  ./yearly                        #performs the scoring
  mv checked.$flag.$RADIUS comments.$flag.$RADIUS fout1.$flag.$RADIUS fout2.$flag.$RADIUS km${RADIUS}
  mv 9? 100 score.9? score.100 km$RADIUS

