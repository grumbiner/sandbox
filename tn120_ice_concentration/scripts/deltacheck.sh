#!/bin/sh

wgrib oldsouthgrib > index.south
wgrib oldnorthgrib > index.north

tag=950914

while [ $tag -le 971031 ]
do

  for pole in north south
  do
    grep $tag index.$pole | wgrib -i old${pole}grib -nh -o $pole
    p=`echo $pole | cut -c1`
    echo $tag >> ${pole}.delta.out
    ./${pole}delta $pole rev/${p}.$tag ${p}land.map >> ${pole}.delta.out
    xpmtogif delta.xpm
    rm delta.xpm
    mv delta.gif ${p}${tag}.gif
  done

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
