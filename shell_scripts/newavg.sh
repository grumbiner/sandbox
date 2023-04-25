#!/bin/sh

set -x
PDY=20070315
COMIN=/com/gfs/prod/gfs.$PDY

#Files are available every 3 hours through the 180
#  then 12 hours through 384 (no need for average, straight wgribbing)
#Note that due to resolution changes, we need to run averager even
#  when files are 12 hours apart

#Ensure that the output files don't already exist
for fn in u.averaged.$PDY v.averaged.$PDY
do
  if [ -f $fn ] ; then
    rm $fn
  fi
done
base=$COMIN/gfs.t00z.sfluxgrb

#Construct averages for u-wind
for hr in 0 12 24 36 48 60 72 84 96 108 120 132 144 168
do
  h1=$hr;
  h2=`expr $h1 + 3`
  h3=`expr $h2 + 3`
  h4=`expr $h3 + 3`
  if [ $h1 -lt 10 ] ; then
    h1=0$h1;
  fi
  if [ $h2 -lt 10 ] ; then
    h2=0$h2;
  fi
  if [ $h3 -lt 10 ] ; then
    h3=0$h3;
  fi
  if [ $h4 -lt 10 ] ; then
    h4=0$h4;
  fi

  wgrib ${base}f$h1 > index
  grep UGRD:kpds5=33:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h1} -nh -o tmpu.$h1.$PDY 
  grep VGRD:kpds5=34:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h1} -nh -o tmpv.$h1.$PDY 

  wgrib ${base}f$h2 > index
  grep UGRD:kpds5=33:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h2} -nh -o tmpu.$h2.$PDY 
  grep VGRD:kpds5=34:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h2} -nh -o tmpv.$h2.$PDY 

  wgrib ${base}f$h3 > index
  grep UGRD:kpds5=33:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h3} -nh -o tmpu.$h3.$PDY 
  grep VGRD:kpds5=34:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h3} -nh -o tmpv.$h3.$PDY 

  wgrib ${base}f$h4 > index
  grep UGRD:kpds5=33:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h4} -nh -o tmpu.$h4.$PDY 
  grep VGRD:kpds5=34:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h4} -nh -o tmpv.$h4.$PDY 

  ./preaverage u.averaged.$PDY tmpu.$h1.$PDY tmpu.$h2.$PDY tmpu.$h3.$PDY tmpu.$h4.$PDY
  ./preaverage v.averaged.$PDY tmpv.$h1.$PDY tmpv.$h2.$PDY tmpv.$h3.$PDY tmpv.$h4.$PDY
done

while [ $hr -lt 384 ]
do
  hr=`expr $hr + 12`
  h1=$hr
  wgrib ${base}f$h1 > index

  grep UGRD:kpds5=33:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h1} -nh -o tmpu.$h1.$PDY 
  ./preaverage u.averaged.$PDY tmpu.$h1.$PDY

  grep VGRD:kpds5=34:kpds6=105:kpds7=10 index | wgrib -i ${base}f${h1} -nh -o tmpv.$h1.$PDY 
  ./preaverage v.averaged.$PDY tmpv.$h1.$PDY
done

rm tmp[uv].*.* index
