#!/bin/sh


FIX=/nwprod/fix
#EXDIR=/nwprod/exec/
EXDIR=/u/wx21rg/rgdev/filter/

ASSESS=time
jday=25
refyear=2005

cd /u/wx21rg/noscrub/ssmi85/

rm -f seaice_*.tab
cp $FIX/*.tab .

PDY=20051211
while [ $PDY -le 20051231 ]
do
  if [ ! -f ssmi.bufr.$PDY ] ; then
    ln -fs ssmi85.$PDY fort.14
    $ASSESS $EXDIR/seaice_ssmibufr >> bufout 2>> bufout
    mv fort.51 ssmi.bufr.$PDY
  fi
  echo ssmi.bufr.$PDY > delta
  echo $PDY

  for sat in 246 248
  do

    $ASSESS $EXDIR/seaice_seaissmi delta $FIX/seaice_nland127.map $FIX/seaice_sland127.map \
       n3ssmi${sat}.$PDY   s3ssmi${sat}.${PDY} \
       umasknorth$sat.$PDY umasksouth$sat.$PDY $jday $refyear $sat > v${sat}.$PDY
  done
  rm ?3ssmi???.$PDY

  PDY=`expr $PDY + 1`
  PDY=`/u/wx21rg/bin/dtgfix3 $PDY`

done
tar cvf ../f13.tar umask*246.*
tar cvf ../f15.tar umask*248.*
