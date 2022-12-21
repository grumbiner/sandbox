#!/bin/sh

#set -x
if [ ! -f ind.2000 ] ; then
  wgrib nhall2000.grib > ind.2000
  wgrib nhavclimo.grib > ind.climo
fi

i=1
while [ $i -lt 31 ]
do
  ftago=20000101
  ftag=`expr $ftago + $i`

  while [ $ftag -le 20001231 ] 
  do
    if [ -f fcst -o -f obs -o -f ref ] ; then
      rm fcst obs ref
    fi
    stag=`echo $ftag | cut -c5-8 `
    tago=`echo $ftago | cut -c3-8`
    tag=`echo  $ftag | cut -c3-8 `
    
    grep $tago ind.2000 | wgrib -i nhall2000.grib -nh -o fcst
    grep $tag ind.2000 | wgrib -i nhall2000.grib -nh -o obs
    grep 00$stag ind.climo | wgrib -i nhavclimo.grib -nh -o ref
  
    if [ -s fcst -a -s obs -a -s ref ] ; then
      echo $tag >> score.${i}day
      ./score fcst obs ref >> score.${i}day 
    fi
  
    ftago=`expr $ftago + 1`
    ftago=`dtgfix3 $ftago`
    ftag=`expr $ftag + 1`
    ftag=`dtgfix3 $ftag`
  done

  i=`expr $i + 1`
done
mv score.*day 2000
nohup time ./2001.sh &
