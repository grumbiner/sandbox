#!/bin/sh


FIX=/nwprod/fix
#EXDIR=/u/wx21rg/rgdev/filter/
EXDIR=/nwprod/exec

ASSESS=time
jday=25
refyear=2009

cd /u/wx21rg/noscrub/ssmi85/

rm -f seaice_*.tab
cp $FIX/*.tab .

PDY=20090101
while [ $PDY -le 20091231 ]
do
  if [ ! -f ssmi.bufr.$PDY ] ; then
    ln -fs ssmi85.$PDY fort.14
    $ASSESS $EXDIR/seaice_ssmibufr >> bufout 2>> bufout
    mv fort.51 ssmi.bufr.$PDY
  fi
  echo  ssmi.bufr.$PDY > delta

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
