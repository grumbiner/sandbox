#!/bin/sh

FCSTDIR=/usr1/modeloutput/driftfore/16days/sk2/
OBSDIR=/export/emc-lw-rgrumbi/rmg3/current/elliott
OUTDIR=ifremer.sk2.scores

for LEAD in 3 6
do
  tag=20020101
  while [ $tag -le 20141231 ]
  do
    if [ -f $OBSDIR/ifremer${LEAD}/${tag}.txt -a -f $FCSTDIR/sk2.$tag ] ; then
      ln -sf $OBSDIR/ifremer${LEAD}/${tag}.txt dboydata
      for RANGE in 55 38.9 25.4 19 12.7 10 5
      do
        if [ ! -f ${OUTDIR}/sk2.$tag.${LEAD}.$RANGE ] ; then
          ./ifremercheck $RANGE 3 ${LEAD} fred.$tag.${LEAD}.$RANGE $FCSTDIR/sk2.$tag > ${OUTDIR}/sk2.$tag.${LEAD}.$RANGE
        fi
      done
    fi
  
    tag=`expr $tag + 1`
    tag=`dtgfix3 $tag`
  done
done
