#!/bin/bash
#Script to:
#  Extract iabp buoys which were 'near' skiles points
#    and follow them for next 16 (forecast_lead) days
#  Construct average information
#  Score forecasts vs. observations

FIXDIR=/usr1/rmg3/drift/fix
IABPDIR=/usr1/data2/2d/buoy/iabp
EXDIR=/usr1/rmg3/drift/score
FORECASTDIR=/usr1/data/modeloutput/driftfore/16days/sk2/

forecast_lead=16
time_range=3.0

if [ ! -f forecast.points ] ; then
  ln -s ${FIXDIR}/forecast.points .
fi

#for RADIUS in 55.0 27.5 110.0 77.5 38.75 165.0 18.0
for RADIUS in 55.0 
do
  if [ ! -d km${RADIUS} ] ; then
    mkdir km${RADIUS}
  fi

  ln -sf ${IABPDIR}/iabp.full dboydata

  time $EXDIR/avg2 $RADIUS $time_range $forecast_lead checked.$RADIUS \
                    fout1.$RADIUS fout2.$RADIUS $FORECASTDIR \
               > avg.$RADIUS
  $EXDIR/splityear.pl < fout2.$RADIUS
  
  #for yy in 98 99 100 101 102 103 104 105 106 
  #do
  #  if [ ! -d km${RADIUS}.$yy ] ; then
  #    mkdir km${RADIUS}.$yy
  #  fi
  #  if [ -f $yy ] ; then
  #    time $EXDIR/score $yy $forecast_lead > score.$RADIUS.$yy 
  #    mv $yy? score.$RADIUS.$yy km$RADIUS.$yy
  #  fi
  #done

  time $EXDIR/score fout2.$RADIUS $forecast_lead > score.$RADIUS

  #mv checked.$RADIUS comments.$RADIUS fout1.$RADIUS \
  #    fout2.$RADIUS km${RADIUS}

done
