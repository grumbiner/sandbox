#!/bin/sh

mo=04
dd=1

while [ $dd -le 28 ] 
do
  if [ $dd -lt 10 ] ; then
    dd=0$dd
  fi
  wget http://data.ncof.co.uk/data/ostia/2010/sst/2010${mo}${dd}-UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc.bz2
  dd=`expr $dd + 1`
done
