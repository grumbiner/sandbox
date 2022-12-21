#!/bin/bash
#Script to:
#  Extract iabp buoys which were 'near' skiles points
#    and follow them for next 16 (forecast_lead) days
#  Construct average information
#  Score forecasts vs. observations
#26 Sep 2014  Robert Grumbine

set -xe

IABPDIR=/usr1/iabp
FORECASTDIR=/usr2/rmg3/modeloutput/driftfore/16days/sk2

EXDIR=`pwd`
FIXDIR=$EXDIR/../fix

for d in $FIXDIR $IABPDIR $EXDIR $FORECASTDIR
do
  if [ ! -d $d ] ; then
    echo do not have directory $d, quitting
    exit 1
  fi
done

forecast_lead=16
time_range=3.0
if [ ! -f iabpcheck -o ! -f avg2 -o ! -f score ] ; then
  make
  make
fi

#Should need no changes below here

if [ ! -f forecast.points ] ; then
  ln -s ${FIXDIR}/seaice_forecast.points forecast.points
fi

for RADIUS in 19.0 27.5 38.9 55.0 77.8 
do
  if [ ! -d km${RADIUS} ] ; then
    mkdir km${RADIUS}
  fi

  if [ ! -f dboydata ] ; then
    ln -sf ${IABPDIR}/iabp.full dboydata
  fi

  time $EXDIR/iabpcheck $RADIUS $time_range $forecast_lead \
                      checked.$RADIUS > comments.$RADIUS
  echo starting avg2
  time $EXDIR/avg2 $RADIUS $time_range $forecast_lead checked.$RADIUS \
                    fout1.$RADIUS fout2.$RADIUS $FORECASTDIR \
               > avg.$RADIUS

  $EXDIR/splityear.pl < fout2.$RADIUS
  
  #for yy in 98 99 100 101 102 103 104 105 106 107 
  for yy in 107 108 109 110 111 112 113 114 115
  do
    if [ ! -d km${RADIUS}.$yy ] ; then
      mkdir km${RADIUS}.$yy
    fi
    if [ -f $yy ] ; then
      time $EXDIR/score $yy $forecast_lead > score.$RADIUS.$yy 
      mv $yy score.$RADIUS.$yy km$RADIUS.$yy
    fi
  done

  time $EXDIR/score fout2.$RADIUS $forecast_lead > score.$RADIUS
  mv checked.$RADIUS comments.$RADIUS fout1.$RADIUS \
      fout2.$RADIUS avg.$RADIUS score.$RADIUS km${RADIUS}

done
