#!/bin/ksh
set -e

TMPDIR=/usr1/dataflow/ice/tmp4
ARCDIR=/usr1/dataflow/unblock/
EXDIR=/usr1/home/wd21rg/newice/
DATDIR=/usr1/home/wd21rg/fix/
export DATDIR ARCDIR EXDIR TMPDIR

#
#Should not need any changes below this point -----------------------
#
if [ ! -d $TMPDIR ] ; then
  mkdir -p $TMPDIR
fi
cd $TMPDIR

for execs in ssmi.oldres.x 
do
  cp $EXDIR/$execs .
done

tag=20000225
end=20031231
#F15 picks up on 2000-03-15
#F16 does not show up until more recently than 12/31/2003.

while [ $tag -le $end ] 
do
  yy=`echo $tag | cut -c1-4`
  mm=`echo $tag | cut -c5-6`
  dd=`echo $tag | cut -c7-8`
  jday=`expr 30 \* $mm`
  jday=`expr $jday + $dd - 30`
  echo $tag
  
  if [ -f $ARCDIR/unblock.$tag ] ; then
    echo $ARCDIR/unblock.$tag > delta
    #Construct the test algorithms
    for satno in 13 14 15 
    do
      time ./ssmi.oldres.x delta $DATDIR/nland.map $DATDIR/sland.map \
      n3ssmi.$tag.$satno s3ssmi.$tag.$satno \
      umasknorth.$tag.$satno umasksouth.$tag.$satno \
      $jday $yy `expr $satno + 233` >> v.$tag.$satno
    done
   else
     echo do not have unblock file for $tag >> script.out
  fi
  
  tag=`expr $tag + 1`
  tag=`/usr1/home/wd21rg/bin/dtgfix3 $tag`

done
