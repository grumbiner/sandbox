#!/bin/sh

tag=19800101
while [ $tag -le 19801231 ]
do
  days=`echo $tag | cut -c5-8`
  time analy/exec/bulk out/ice.????$days > out.$days
  mv count count.$days

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done

#exit

for mm in 01 02 03 04 05 06 07 08 09 10 11 12
do
  time analy/exec/bulk out/ice.????${mm}?? > out.$mm
  mv count count.$mm
done

yy=1981
#yy=1992
while [ $yy -le 2010 ]
do
  time analy/exec/bulk out/ice.${yy}???? > out.$yy
  mv count count.$yy
  yy=`expr $yy + 1`
done

time analy/exec/bulk out/ice.???????? > out.all
mv count count.all

