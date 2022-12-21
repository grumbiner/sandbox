#!/bin/sh
#Robert Grumbine 20 April 2004

#CC *.C -DSGI *.o -lm -I ~/includes ~/nwprod/w3lib_rest -o regress
rm matchup.*

DDIR=/usr1/dataflow/ice/tmp4/

tag=20000315
while [ $tag -le 20031231 ]
do
  ./regress sland ${DDIR}/s3ssmi.$tag.13 ${DDIR}/s3ssmi.$tag.14 ${DDIR}/s3ssmi.$tag.15 20 > reg.$tag
  mkdir $tag
  mv matchup* reg.$tag $tag

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
