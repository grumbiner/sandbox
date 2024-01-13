#!/bin/sh
#Script to generate meteorological files for the sea ice model
#Robert Grumbine 24 May 2002

EXDIR=/usr1/rmg3/icemodel/execs

for pole in south north
do
  export pole EXDIR
  
  tag=20001101
  while [ $tag -le 20020515 ]
  do
    for hh in 00 06 12 18
    do
      if [ -f flxf06.${tag}$hh ] ; then
        $EXDIR/met.sh flxf06.${tag}$hh
        mv metout.$pole metout.${pole}.${tag}$hh
      fi
    done
    tag=`expr $tag + 1`
    tag=`dtgfix3 $tag `
  done
done

