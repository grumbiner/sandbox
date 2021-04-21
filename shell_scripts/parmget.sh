#!/bin/sh

set -x

export tag=2018042700
source=http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.$tag

hr=00
while [ $hr -le 240 ]
do
  get_inv.pl $source/gfs.t00z.sfluxgrbf${hr}.grib2.idx | grep -f parms | \
  get_grib.pl $source/gfs.t00z.sfluxgrbf${hr}.grib2 test.grib2.$tag.f$hr 

  hr=`expr $hr + 3`
  if [ $hr -lt 10 ] ; then
    hr=0$hr
  fi

done

hr=252
while [ $hr -le 384 ]
do
   ./get_inv.pl $source/gfs.t00z.sfluxgrbf${hr}.grib2.idx | grep -f parms | \
  ./get_grib.pl $source/gfs.t00z.sfluxgrbf${hr}.grib2 test.grib2.$tag.f$hr 

  hr=`expr $hr + 12`
  if [ $hr -le 10 ] ; then
    hr=0$hr
  fi

done
