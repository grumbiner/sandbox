#!/bin/sh
# @account_no=WAV-MTN
set -xeu
#
#WORKHOME=/marine/save/wx21vg/grib2/seaice/encode
#datain=/marine/save/wx21vg/grib2/seaice/encode/data
#WORKTMP=/stmp/wx21vg/grib2sice.sh

WORKHOME=/u/wx21rg/rgdev/grib2
datain=$WORKHOME/data
WORKTMP=$WORKHOME/tmp

mkdir -p $WORKTMP
cd $WORKTMP
#
export XLFRTEOPTS="unit_vars=yes"
#

HR="00"
#cp /com/date/t${HR}z idate
echo 'DATE  20120328000000WASHINGTON' > idate

if [ ! -f seaice_pds ] ; then
  cp $WORKHOME/seaice_pds      .
fi
if [ -f infile ] ; then
  rm -f infile
fi

#export names='5min.grb grb north12psg northpsg south12psg southpsg'
export fn='umasknorth umasksouth fill umasknorth12 umasksouth12 fill5min'
#
for name in $fn
do
  cp $WORKHOME/seaice.t00z.${name}   infile
  #
  export XLFUNIT_4="idate"
  export XLFUNIT_10="infile"
  export XLFUNIT_20="devgrib2.${name}"

  export XLFUNIT_12="$WORKHOME/gds.${name}"
  
  timex $WORKHOME/seaice_grib2.x 1> ft06.${name} 2> errfile.$name
  rm -f infile
#
done
