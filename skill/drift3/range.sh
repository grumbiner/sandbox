#!/bin/sh
#2 April 2014  Robert Grumbine

FCSTDIR=/usr1/modeloutput/driftfore/16days/sk2
OBSDIR=/export/emc-lw-rgrumbi/rmg3/current/drift3
LEAD=6

tag=20021001
while [ $tag -le 20140331 ]
do
  if [ -f $FCSTDIR/sk2.$tag -a -f $OBSDIR/ifremer${LEAD}/${tag}.txt ] ; then 
    ln -sf $OBSDIR/ifremer${LEAD}/${tag}.txt dboydata
    for RANGE in 155 110 77.8 55.0 38.9 27.5 20 15
    do
      ./ifremercheck $RANGE 3 ${LEAD} fred.$tag.${LEAD}.$RANGE $FCSTDIR/sk2.$tag > a.$tag.${LEAD}.$RANGE
    done
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
