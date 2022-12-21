#!/bin/ksh
#Script to get meteorological forcing either for 'today' or for a date
#  passed in.  Note that 4 digit year is required
#Robert Grumbine 19 March 1999
#Generalized some more 8 December 1999

REMID=wd21rg
REMSITE=cray4
BASE=/global/prs
FBASE=${FBASE:-~wd21rg/archive/icemodel/forcing/fcst}
HBASE=${HBASE:-~wd21rg/archive/icemodel/forcing/hcst}

########---------Should need no change below here --------------

for d in $FBASE $HBASE
do
  if [ ! -d $d ] ; then
    mkdir -p $d
  fi
done

if [ $# -eq 1 ] ; then
  tag=$1
else
  tag=`date +'%Y%m%d' `
fi
#get the forecast files
rcp $REMID@${REMSITE}:$BASE/flxf06.${tag}00 ${FBASE}/flxf06a.$tag
rcp $REMID@${REMSITE}:$BASE/flxf06.${tag}06 ${FBASE}/flxf06b.$tag
#for time in 12 24 36 48 60 72 84 96 108 120 132 144 156 168 \
#            180 192 204 216 228 240 
#            180 192 204 216 228 240 252 264 276 288 300 312 \
#            324 336 348 360 372 384
for time in 12 24 36 48 60 72 84 96 108 120 
do
  if [ ! -f ${FBASE}/flxf${time}.${tag}00 ] ; then
    rcp $REMID@${REMSITE}:$BASE/flxf${time}.${tag}00 ${FBASE}
  fi
done

#get the hindcast files
cp ${FBASE}/flxf06a.$tag $HBASE/flxf06.${tag}00
cp ${FBASE}/flxf06b.$tag $HBASE/flxf06.${tag}06
rcp $REMID@${REMSITE}:$BASE/flxf06.${tag}12 ${HBASE}
rcp $REMID@${REMSITE}:$BASE/flxf06.${tag}18 ${HBASE}
