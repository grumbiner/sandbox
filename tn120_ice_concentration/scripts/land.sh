#!/bin/sh

cd /u/wx21rg/noscrub/rfc1.2011
export OUTDIR=/u/wx21rg/noscrub/rfc1.2011
export EXDIR=/u/wx21rg/rgdev/ice1/execs
export BASE=/u/wx21rg/rgdev/ice1/

#Land mask intercomparisons
for type in ops rfc
do
  PDY=20100214
  PDYm1=20100213
  export FIX=${BASE}/${type}.fix
  
  while [ $PDY -le 20110213 ]
  do
  
    wgrib /u/wx21rg/noscrub/sst/rtgssthr_grb_0.083.$PDY | \
      wgrib -i /u/wx21rg/noscrub/sst/rtgssthr_grb_0.083.$PDY -nh -o sst

#Testing for effects of changed hemispheric land masks
    $EXDIR/seaice_filtanal sst umasknorth12.${PDY} umasksouth12.${PDY} \
          ${type}latlon.$PDY nps.$PDY sps.$PDY $FIX/seaice_gland5min 275.3 \
          $FIX/seaice_nland127.map $FIX/seaice_sland127.map

    $EXDIR/seaice_icegrid ${type}fill5min.$PDYm1 ${type}latlon.$PDY ${type}age.$PDYm1 \
                             ${type}age.$PDY ${type}fill5min.$PDY

    $EXDIR/seaice_monitor_c12th  ${type}fill5min.$PDY ${type}fill5min.$PDYm1  \
                           $FIX/seaice_gland5min ${type}seaice_delta_$PDY.xpm \
                           ${type}seaice_monitor_${PDY}.kml \
                           > ${type}seaice_monitor_${PDY}.txt

  
  
    PDYm1=$PDY
    PDY=`expr $PDY + 1`
    PDY=`dtgfix3 $PDY`

  done
  rm nps.* sps.*
  tar cvf ../${type}.tar ${type}*
  rm ${type}*
done
