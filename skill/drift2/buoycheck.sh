#!/bin/bash
#Script to scan buoy files for drift information.
#Parameters:
#  RADIUS = radius in km, within which a buoy must be to a skiles point
#  HOURS  = time window (w.r.t. 00 UTC)
#  DAYS   = date range for forecasting
#  BUOYDIR = name of directory that has buoy files

RADIUS=55.0
HOURS=3.0
DAYS=16
BUOYDIR=buoys
export RADIUS HOURS DAYS BUOYDIR

for yr in 1995 1996 1997 1998 1999
do
  if [ -f dboydata ] ; then
    rm dboydata
  fi
  ln -s $BUOYDIR/dboydata.$yr dboydata
  if [ ! -s checked.$yr ] ; then
    time ./buoycheck2 $RADIUS $HOURS $DAYS checked.$yr > comments.$yr
    gprof buoycheck2 > prof.$yr
  fi
done
