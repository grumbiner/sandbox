#!/bin/bash

#ftp://eclipse.ncdc.noaa.gov/pub/OI-daily/IEEE
x=1986
while [ $x -le 2007 ]
do
  if [ ! -d $x ] ; then
    mkdir $x
  fi

  cd $x
  #echo anonymous > l
  #echo rmg3@ >> l
  echo cd pub/OI-daily/IEEE > l
  echo cd $x/AVHRR >> l
  echo binary >> l
  echo prompt >> l
  echo mget '*' >> l
  echo quit >> l
  cat l | ftp eclipse.ncdc.noaa.gov 2>> sst.err >> sst.out
  cd ..

  x=`expr $x + 1`
done
