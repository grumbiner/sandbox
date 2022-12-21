#!/bin/sh

ln -sf skip_lowres skip

i=0
yy=2000
while [ $yy -le 2011 ]
do
  for mo in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    wgrib hr/ice.${yy}${mo} > index 
    x=`wc -l index`
    cat index | wgrib -i -nh hr/ice.${yy}${mo} -o alpha
    y=`./avg2 alpha $x` 
    i=`expr $i + 1`
    echo $i $yy $mo $y >> avg2.out
  done
  yy=`expr $yy + 1`
done
