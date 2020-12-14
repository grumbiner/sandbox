#!/bin/ksh

#First pass: extract ice fields and filter with respect to sst
tag=19950920
while [ $tag -le 20021031 ] 
#while [ $tag -le 19950930 ] 
do
  yy=`echo $tag | cut -c1-4`
  y2=`echo $tag | cut -c3-8`
  #echo $yy $y2
  if [ ! -f sst.index.$yy ] ; then
    wgrib sst.$yy > sst.index.$yy
  fi
  if [ ! -f npsg.index.$yy ] ; then
    wgrib npsg.$yy > npsg.index.$yy
  fi
 
  grep $y2 npsg.index.$yy | wgrib -i npsg.$yy -nh -o orice.$tag
  #note that the following will persist the last sst across gaps
  grep $y2 sst.index.$yy | grep TMP | wgrib -i sst.$yy -nh -o sst_tmp
  if [ -s sst_tmp ] ; then
    mv sst_tmp sst
  fi

  if [ -s orice.$tag ] ; then
    ./baseclimo orice.$tag sst nland filtice.$tag >> baseout
  else
    echo no ice for $tag >> baseout
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
