#!/bin/sh

cd /u/wx21rg/noscrub/rfc1.2011
export OUTDIR=/u/wx21rg/noscrub/rfc1.2011
export EXDIR=/u/wx21rg/rgdev/ice1/rfc2.2011.execs/
export FIX=/u/wx21rg/rgdev/ice1/rfc2.2011.fix/

#Test the posteriori filtering
tag4=20100214
tagm=20100213
  cp $FIX/seaice_posteriori_5min .

#Compute fields for the reference, ensuring same sst, land masks, and filtanal versions:
while [ $tag4 -le 20110213 ]
do

  wgrib /u/wx21rg/noscrub/sst/rtgssthr_grb_0.083.$tag4 | \
    wgrib -i /u/wx21rg/noscrub/sst/rtgssthr_grb_0.083.$tag4 -nh -o sst
  $EXDIR/seaice_filtanal sst umasknorth12.${tag4} umasksouth12.${tag4} \
        reflatlon.$tag4 nps.$tag4 sps.$tag4 $FIX/seaice_gland5min 275.3 \
        $FIX/seaice_nland127.map $FIX/seaice_sland127.map


  tagm=$tag4
  tag4=`expr $tag4 + 1`
  tag4=`dtgfix3 $tag4`

done
rm nps.* sps.*
tar cvf ../post2.tar reflatlon.*
