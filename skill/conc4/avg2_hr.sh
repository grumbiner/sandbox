#!/bin/sh

#set -xe
ln -sf skip_hr skip

i=0
yy=2004
while [ $yy -le 2011 ]
do
  for mo in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    x=0
    if [ -f hr/ice5min.${yy}${mo} ] ; then
      wgrib hr/ice5min.${yy}${mo} > index 
      x=`wc -l index | cut -f1 -d\ `
      if [ $x -gt 0 ] ; then
        cat index | wgrib -i -nh hr/ice5min.${yy}${mo} -o alpha
        y=`./avg2_hr alpha $x` 
        i=`expr $i + 1`
        echo $i $yy $mo $y >> avg2_hr.out
      fi
    fi
    rm index
  done
  yy=`expr $yy + 1`
done
