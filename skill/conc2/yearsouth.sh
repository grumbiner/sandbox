#!/bin/sh

for yy in 96 97 98 99
do
  i=1
  while [ $i -lt 31 ]
  do
    tago=${yy}0101
    tag=`expr $tago + $i`

  while [ $tag -le ${yy}1231 -a $tag -gt 950101 ] 
  do
    if [ -f fcst -o -f obs -o -f ref ] ; then
      rm fcst obs ref
    fi
    stag=`echo $tag | cut -c3-6 `
    grep $tago ind.${yy} | wgrib -i shall${yy}.grib -nh -o fcst
    grep $tag ind.${yy} | wgrib -i shall${yy}.grib -nh -o obs
    grep 00$stag ind.climo | wgrib -i shavclimo.grib -nh -o ref
  
    if [ -s fcst -a -s obs -a -s ref ] ; then
      echo $tag >> score.${i}day
      ./scoresouthper fcst obs ref sland >> score.${i}day 
    fi
  
    tago=`expr $tago + 1`
    tago=`dtgfix3 $tago`
    tag=`expr $tag + 1`
    tag=`dtgfix3 $tag`
  done

  i=`expr $i + 1`
done #with going out to 31 day lead

mv score.*day s${yy}
done
