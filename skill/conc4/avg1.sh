#!/bin/sh

ln -sf skip_lowres skip

yy=1978
i=0
while [ $yy -le 2007 ]
do
  for mo in 01 02 03 04 05 06 07 08 09 10 11 12
  do
    if [ -f cfsrr/splice6.${yy}${mo}28 ] ; then
      i=`expr $i + 1`
      y=`./avg1 cfsrr/splice6.${yy}${mo}?? `
#>> avg1.out
      echo $i $yy $mo $y >> avg1.out
    fi
  done
  yy=`expr $yy + 1`
done
