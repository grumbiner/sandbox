#!/bin/sh
#Script to generate meteorological files for the sea ice model
#Robert Grumbine 24 May 2002

EXDIR=/usr1/rmg3/icemodel/execs
MET=/usr1/sda/icemet/

#set -x
for pole in north south 
do
  tag=20000101
  export pole EXDIR
  
  while [ $tag -le 20001231 ]
  do
    for hh in 00 06 12 18
    do
      if [ -f flxf06.${tag}$hh -a ! -f $MET/metout.${pole}.${tag}$hh ] ; then
        $EXDIR/met.sh flxf06.${tag}$hh
        mv metout.$pole $MET/metout.${pole}.${tag}$hh
      fi
    done
    tag=`expr $tag + 1`
    tag=`dtgfix3 $tag `
  done
done
