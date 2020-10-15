#!/bin/sh

#set -x

for yy in 96 97 98 99 2000 2001
do
  if [ ! -f ind.$yy ] ; then
    wgrib shall$yy.grib > ind.$yy
  fi
done
if [ ! -f ind.climo ] ; then
  wgrib shavclimo.grib > ind.climo
fi

base=961001
while [ $base -le 970531 ]
do
  i=0

  for hh in 24 48 72 96 120 144 168
  do
    i=`expr $i + 1`
    tagv=`expr $base + 1`
    tagv=`dtgfix3 $tagv`
  
    if [ -f fcst -o -f obs -o -f ref ] ; then
      rm fcst obs ref
    fi
    
    wgrib ../modelout/southgrib.$base.f$hh | grep ICEC | \
        wgrib -i ../modelout/southgrib.$base.f$hh -nh -o fcst

    stag=`echo $tagv | cut -c3-6 `
    yy=`echo $tagv | cut -c1-2`
    grep $tagv ind.$yy | wgrib -i shall$yy.grib -nh -o obs
    #grep 00$stag ind.climo | wgrib -i shavclimo.grib -nh -o ref
    grep 98$stag ind.98 | wgrib -i shall98.grib -nh -o ref
  
    if [ -s fcst -a -s obs -a -s ref ] ; then
      echo f$base$hh >> south98.out
      ./scoresouthmodel fcst obs ref sland >> south98.out
      #mv fcst.xpm fcst${base}${hh}.xpm
      #mv ref.xpm  ref${base}${hh}.xpm
      #mv obs.xpm  obs${base}${hh}.xpm
    fi
  
  done
  base=`expr $base + 1`
  base=`dtgfix3 $base`

done
