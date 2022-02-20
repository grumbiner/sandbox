#!/bin/sh

cd /u/wx21rg/noscrub/rfc1.2011
export OUTDIR=/u/wx21rg/noscrub/rfc1.2011
export EXDIR=/u/wx21rg/rgdev/ice1/rfc2.2011.execs/
export FIX=/u/wx21rg/rgdev/ice1/rfc2.2011.fix/

#New -- monitor files against yesterday, and product an ice edge point
#   set for use by drift model
tag4=20100214
tagm=20100213
while [ $tag4 -le 20110213 ]
do

  $EXDIR/seaice_monitor_c12th  fill5min.$tag4 ${OUTDIR}/fill5min.$tagm  \
                           $FIX/seaice_gland5min seaice_delta_$tag4.xpm \
                           seaice_monitor_${tag4}.kml \
                           > seaice_monitor_${tag4}.txt
  $EXDIR/seaice_edge fill5min.$tag4 $FIX/seaice_gland5min \
                     seaice_edge_${tag4}.kml > seaice_edge_${tag4}.txt
  tagm=$tag4
  tag4=`expr $tag4 + 1`
  tag4=`dtgfix3 $tag4`

done
tar cvf ../edgemontest.tar seaice_delta_*.xpm *.kml *.txt
