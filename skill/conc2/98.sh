#!/bin/sh

#set -x
if [ ! -f ind.98 ] ; then
  wgrib nhall98.grib > ind.98
  wgrib nhavclimo.grib > ind.climo
fi

i=1
while [ $i -lt 31 ]
do
  tago=980101
  tag=`expr $tago + $i`

  while [ $tag -le 981231 ] 
  do
    if [ -f fcst -o -f obs -o -f ref ] ; then
      rm fcst obs ref
    fi
    stag=`echo $tag | cut -c3-6 `
    grep $tago ind.98 | wgrib -i nhall98.grib -nh -o fcst
    #grep 98$stag ind.98 | wgrib -i nhall98.grib -nh -o obs
    grep $tag ind.98 | wgrib -i nhall98.grib -nh -o obs
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
mv score.*day 1998
./99.sh
