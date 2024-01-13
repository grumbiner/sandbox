#!/bin/sh

#set -x

tag=20090210

while [ $tag -le 20090428 ] 
do
  base=`echo $tag | cut -c1-6`
  if [ ! -f nindex.$base ] ; then
    wgrib n12psg.$base > nindex.$base
    wgrib s12psg.$base > sindex.$base
  fi

  stag=`echo $tag | cut -c3-8`
  grep $stag nindex.$base | wgrib -i n12psg.$base -nh -o nssmi > /dev/null
  grep $stag sindex.$base | wgrib -i s12psg.$base -nh -o sssmi > /dev/null

  echo $tag
  ./blend nchar.$tag nssmi nout.$tag schar.$tag sssmi sout.$tag /usr/local/data/seaice_nland127.map /usr/local/data/seaice_sland127.map 

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`

done
