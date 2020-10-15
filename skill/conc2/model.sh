#!/bin/sh

#set -x
if [ ! -f ind.96 ] ; then
  wgrib nhall96.grib > ind.96
  wgrib nhall97.grib > ind.97
  wgrib nhavclimo.grib > ind.climo
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
    
    wgrib ../modelout/northgrib.$base.f$hh | grep ICEC | \
        wgrib -i ../modelout/northgrib.$base.f$hh -nh -o fcst

    stag=`echo $tagv | cut -c3-6 `
    yy=`echo $tagv | cut -c1-2`
    grep $tagv ind.$yy | wgrib -i nhall$yy.grib -nh -o obs
    grep 00$stag ind.climo | wgrib -i nhavclimo.grib -nh -o ref
    #grep 98$stag ind.98 | wgrib -i nhall98.grib -nh -o ref
  
    if [ -s fcst -a -s obs -a -s ref ] ; then
      echo f$base$hh >> scoreclim.out
      ./scoremodel fcst obs ref nland >> scoreclim.out
      mv fcst.xpm fcst${base}${hh}.xpm
      mv ref.xpm  ref${base}${hh}.xpm
      mv obs.xpm  obs${base}${hh}.xpm
    fi
  
  done
  base=`expr $base + 1`
  base=`dtgfix3 $base`

done
