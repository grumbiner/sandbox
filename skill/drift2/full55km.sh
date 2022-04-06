#!/bin/bash
#Script for full scoring of drift model:
#  scan buoy files for drift information - buoycheck2
#  average multiple reports near 00 UTC,
#     and match up to forecasts          - avg2
#  conduct scoring                       - score
#
#Parameters:
#  RADIUS = radius in km, within which a buoy must be to a skiles point
#  HOURS  = time window (w.r.t. 00 UTC)
#  DAYS   = date range for forecasting
#  BUOYDIR = name of directory that has buoy files
#
#Robert Grumbine 9 November 1999

RADIUS=55.0
HOURS=3.0
DAYS=16
BUOYDIR=buoys
export RADIUS HOURS DAYS BUOYDIR

#---------- Should not need to change anything below here -----------

for yr in 1995 1996 1997 1998 1999
do
#Buoy file checking
  if [ -f dboydata ] ; then
    rm dboydata
  fi
  ln -s $BUOYDIR/dboydata.$yr dboydata
  if [ ! -s checked.$yr ] ; then
    time ./buoycheck2 $RADIUS $HOURS $DAYS checked.$yr > comments.$yr
    gprof buoycheck2 > prof.$yr
  fi

  if [ ! -s fout2.$yr ] ; then
    ./avg2 $RADIUS $HOURS $DAYS checked.$yr fout1.${yr} fout2.${yr}
  fi

  yy=`echo $yr | cut -c3-4`
  ./score fout2.$yr $DAYS $yy > scores.$yr
done
