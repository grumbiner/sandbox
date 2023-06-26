#!/bin/sh

wgrib oldglobalgrib > index.global

tag=951209

while [ $tag -le 971030 ]
do

  for pole in global
  do
    grep $tag index.$pole | wgrib -i old${pole}grib -nh -o $pole
    p=`echo $pole | cut -c1`
    echo $tag >> ${pole}.delta.out
    ./${pole}delta $pole /data/forecasts/reproc/fill.$tag /home/wd21rg/programming/fix/global >> ${pole}.delta.out
    xpmtogif delta.xpm
    rm delta.xpm
    mv delta.gif ${p}${tag}.gif
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
