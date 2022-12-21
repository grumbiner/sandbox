#!/bin/bash
#Script to split out drift verification figures for sea ice model scoring

set -x

#Loop through for different ranges 
flag=full
for RADIUS in 19.0 27.5 40.0 55.0 80.0 110.0 165.0 
do
  if [ ! -d km${RADIUS} ] ; then
    exit 1
  fi
  ./splityear.pl < km${RADIUS}/fout2.$flag.$RADIUS
  ./yearly                        #performs the scoring
  mv 9? 100 score.9? score.100 km$RADIUS

done
