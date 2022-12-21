#!/bin/sh

#set -x
if [ ! -f ind.96 ] ; then
  wgrib nhall96.grib > ind.96
  wgrib nhall97.grib > ind.97
  wgrib nhavclimo.grib > ind.climo
fi

i=1
while [ $i -lt 31 ]
do
  tago=970101
  tag=`expr $tago + $i`

  while [ $tag -le 971231 ] 
  do
    if [ -f fcst -o -f obs -o -f ref ] ; then
      rm fcst obs ref
    fi
    stag=`echo $tag | cut -c3-6 `
    grep $tago ind.97 | wgrib -i nhall97.grib -nh -o fcst
    #grep 97$stag ind.97 | wgrib -i nhall97.grib -nh -o obs
    grep $tag ind.97 | wgrib -i nhall97.grib -nh -o obs
    grep 00$stag ind.climo | wgrib -i nhavclimo.grib -nh -o ref
  
    if [ -s fcst -a -s obs -a -s ref ] ; then
      echo $tag >> score.${i}day
      ./score fcst obs ref >> score.${i}day 
    fi
  
    tago=`expr $tago + 1`
    tago=`dtgfix3 $tago`
    tag=`expr $tag + 1`
    tag=`dtgfix3 $tag`
  done

  i=`expr $i + 1`
done
