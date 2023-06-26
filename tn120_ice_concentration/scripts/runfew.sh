#!/bin/sh

set -x

tag=20100214
while [ $tag -le 20110213 ]
do
  yy=`echo $tag | cut -c1-4`
  rest=`echo $tag | cut -c5-8`
  ./few.sh $yy $rest > outfew.$tag
  tag=`expr $tag + 1`
  tag=`/u/wx21rg/bin/dtgfix3 $tag`
done
