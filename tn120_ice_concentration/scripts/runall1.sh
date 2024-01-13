#!/bin/sh

tag=20101008
while [ $tag -le 20110214 ]
do
  yy=`echo $tag | cut -c1-4`
  rest=`echo $tag | cut -c5-8`
  ./runamsr1.sh $yy $rest > out1.$tag
  tag=`expr $tag + 1`
  tag=`/u/wx21rg/bin/dtgfix3 $tag`
done
