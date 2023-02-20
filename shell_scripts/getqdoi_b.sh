#!/bin/bash

#ftp://eclipse.ncdc.noaa.gov/pub/OI-daily/IEEE

  cd 2006
  #echo anonymous > l
  #echo rmg3@ >> l
  echo cd pub/OI-daily/IEEE/2006/AVHRR > l
  echo binary >> l
  echo prompt >> l
  echo mget '20060[6-9]*' >> l
  echo mget '20061*' >> l
  echo lcd ../2007 >> l
  echo cd pub/OI-daily/IEEE/2007/AVHRR >> l
  echo mget '*' >> l
  echo quit >> l
  cat l | ftp eclipse.ncdc.noaa.gov 2>> sst.err >> sst.out
  cd ..

