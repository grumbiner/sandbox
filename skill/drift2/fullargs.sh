#!/bin/bash
#Script for full scoring of drift model:
#  scan buoy files for drift information - buoycheck
#  average multiple reports near 00 UTC,
#     and match up to forecasts          - avg2
#  conduct scoring                       - score
#
#Parameters may be specified in arguments to the script
#Parameters:
#  RADIUS = radius in km, within which a buoy must be to a skiles point
#  HOURS  = time window (w.r.t. 00 UTC)
#  DAYS   = date range for forecasting
#  BUOYDIR = name of directory that has buoy files
#
#Requires the skiles model 'forecast.points' file to be in same directory.
#
#Robert Grumbine 9 November 1999
#Arguments added 3 April 2000

if [ $# -ge 4 ] ; then
  RADIUS=$1
  HOURS=$2
  DAYS=$3
  BUOYDIR=$4
else
  RADIUS=55.0
  HOURS=3.0
  DAYS=16
  BUOYDIR=buoys
fi
export RADIUS HOURS DAYS BUOYDIR

#---------- Should not need to change anything below here -----------

for yr in 1993 1994 1995 1996 1997 1998 1999 2000
do
#Buoy file checking
  if [ -f dboydata ] ; then
    rm dboydata
  fi
  ln -s $BUOYDIR/dboydata.$yr dboydata
  if [ ! -s checked.$yr ] ; then
    time ./buoycheck $RADIUS $HOURS $DAYS checked.$yr > comments.$yr
    gprof buoycheck > prof.$yr
  fi

  if [ ! -s fout2.$yr ] ; then
    ./avg2 $RADIUS $HOURS $DAYS checked.$yr fout1.${yr} fout2.${yr}
  fi

  yy=`echo $yr | cut -c3-4`
  ./score fout2.$yr $DAYS $yy > scores.$yr
done
