#!/bin/sh

tag=20021001
while [ $tag -le 20030921 ]
do
  y1=`echo $tag | cut -c1-4`
  y2=`expr $y1 + 1`
  if [ ! -f npsg.index.$y1 ] ; then
    wgrib npsg.$y1 > npsg.index.$y1
  fi
  if [ ! -f npsg.index.$y2 ] ; then
    wgrib npsg.$y2 > npsg.index.$y2
  fi
  if [ ! -f rtg_sst.index.$y1 ] ; then
    wgrib rtg_sst.$y1 > rtg_sst.index.$y1
  fi
  if [ ! -f rtg_sst.index.$y2 ] ; then
    wgrib rtg_sst.$y2 > rtg_sst.index.$y2
  fi

  base=`echo $tag | cut -c5-8`
  lbase=`echo $tag | cut -c3-8`
  base2=`echo $y2 | cut -c3-4`
  base2=${base2}${base}

  rm sst.old sst.new ice.old ice.new
  wgrib rtg_sst.$y1 | grep $lbase | wgrib -i rtg_sst.$y1 -nh -o sst.old 
  wgrib rtg_sst.$y2 | grep $base2 | wgrib -i rtg_sst.$y2 -nh -o sst.new 
  wgrib npsg.$y1 | grep $lbase | wgrib -i npsg.$y1 -nh -o ice.old
  wgrib npsg.$y2 | grep $base2 | wgrib -i npsg.$y2 -nh -o ice.new

  if [ -s ice.old -a -s ice.new -a -s sst.old -a -s sst.new ] ; then
    ./score ice.old avg.$base sst.old ice.new a365 out.$tag sst.new >> score.out
  else
    echo missing data on $tag
  fi

  tag=`expr $tag + 1`
  tag=`dtgfix3 $tag`
done
