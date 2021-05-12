#!/bin/sh

parm=pressfc
for parm in wnd10m tmp2m
do
  yy=1979
  while [ $yy -le 2009 ]
  do
    for mm in 01 02 03 04 05 06 07 08 09 10 11 12
    do
      if [ ! -f ${parm}.l.gdas.${yy}${mm}.grb2 ] ; then
        wget  ftp://nomads.ncdc.noaa.gov/CFSR/HP_time_series/${yy}${mm}/${parm}.l.gdas.${yy}${mm}.grb2
      fi
    done
    yy=`expr $yy + 1`
  done
done

