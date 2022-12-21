#!/bin/ksh

set -x

TMPDIR=/usr1/dataflow/ice/tmp
if [ ! -d $TMPDIR ] ; then
  mkdir -p $TMPDIR
fi
cd $TMPDIR
ARCDIR=/usr1/dataflow/
EXDIR=/usr1/home/wd21rg/
DATDIR=/usr1/home/wd21rg/fix/
export DATDIR ARCDIR EXDIR TMPDIR

#
#Should not need any changes below this point -----------------------
#
for clear in fort.51 fort.14
do
  if [ -f $clear ] ; then
    rm $clear
  fi
done 

tag=`date +"%Y%m%d"`
tag=20010611

for execs in ssmi.bufr.x ssmi 
do
  cp $EXDIR/$execs .
done

#Get and process the bufr tank into something readable
if [ ! -f $ARCDIR/ssmi85/ssmi85.$tag ] ; then
  echo Could not find ssmi85.$tag exiting
  exit 1
else
  ln -s $ARCDIR/ssmi85/ssmi85.$tag fort.14
fi
if [ ! -f fort.14 ] ; then
  echo failed to construct an ssmi input file!!
  exit 1
fi
time ./ssmi.bufr.x
rm fort.14

#Construct the test algorithms
time $EXDIR/ssmi delta $DATDIR/nland.map $DATDIR/sland.map \
  n3ssmi.$tag4.14 s3ssmi.$tag4.14 umasknorth.$tag4.14 umasksouth.$tag4.14 \
  b3n.14 b3s.14 $jday $yy 247 >> v.14

time $EXDIR/ssmi delta $DATDIR/nland.map $DATDIR/sland.map \
  n3ssmi.$tag4.13 s3ssmi.$tag4.13 umasknorth.$tag4.13 umasksouth.$tag4.13 \
  b3n.13 b3s.13 $jday $yy 246 >> v.13

time $EXDIR/ssmi delta $DATDIR/nland.map $DATDIR/sland.map \
  n3ssmi.$tag4.11 s3ssmi.$tag4.11 umasknorth.$tag4.11 umasksouth.$tag4.11 \
  b3n.11 b3s.11 $jday $yy 244 >> v.11
