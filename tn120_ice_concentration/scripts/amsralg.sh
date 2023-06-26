#!/bin/sh

ASSESS=time
PDY=20100214
BASE=/u/wx21rg/rgdev/ice1/

FIX=$BASE/rfc.fix

cd /u/wx21rg/noscrub/amsre/

while [ $PDY -le 20110214 ]
do

  for type in ops rfc
  do
    EXDIR=$BASE/${type}.execs

    rm -f seaice_*.tab.amsr
    cp $FIX/*.tab.amsr .

    $ASSESS $EXDIR/seaice_seaiamsr amsr.bufr.$PDY $FIX/seaice_nland127.map $FIX/seaice_sland127.map \
       ${type}n3amsr.$PDY       ${type}s3amsr.${PDY} \
       ${type}umasknorth12.$PDY ${type}umasksouth12.$PDY > ${type}v.$PDY
  done

  PDY=`expr $PDY + 1`
  PDY=`/u/wx21rg/bin/dtgfix3 $PDY`

done
tar cvf ../opsalg.tar ops?3amsr.* opsumask*12.*
tar cvf ../rfcalg.tar rfc?3amsr.* rfcumask*12.*
