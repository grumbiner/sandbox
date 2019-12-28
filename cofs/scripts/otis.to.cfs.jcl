#!/bin/bash
#Script to read in the layers of the otis analysis and interpolate them
#  on to the cfs grid.  This is run automatically by the cron job which
#  archives the otis fields in the first place.  This script can
#  only be run by me (rmg3) but is included here for informational 
#  purposes.
#Robert Grumbine 15 August 1997

if [ ! -d /tmp/otis ] ; then
  mkdir -p /tmp/otis
fi
cd /tmp/otis
rm *

cp ~/cpp/inhts.* .
cp ~/cpp/hotis .
cp ~/cpp/votis ./votis
gunzip *gz

#tag=`date +"%y%m%d"`
#echo $tag
tag=$1
tar xvf /export/s2/otis/$tag.tar > /dev/null
rm TEMP*
cat ???X*GRB > otis.${tag}.grib
wgrib otis.${tag}.grib > index
#set -x

for level in 1 3 8 13 18 25 33 40 50 63 75 100 125 150 200 300 400 500 600 \
             700 800 900 1000 1100 1500 1750 2000 2500 3000 4000 5000
do
  x=`grep "kpds7=${level}:" index | grep TMP | wc -l`
  y=`grep "kpds7=${level}:" index | grep SALTY | wc -l`
  
    if [ $x -eq 1 ] ; then
      grep "kpds7=${level}:" index | grep TMP | wgrib otis.${tag}.grib -i -o tmp.$level -nh > /dev/null
      ./hotis tmp.$level tmp $level
     else
      echo Should have found 1 field for level $level Temperatures, found $x
      echo cannot retrieve this for later use.  Create a dummy level.
    fi
  
    if [ $y -eq 1 ] ; then
      grep "kpds7=${level}:" index | grep SALTY | wgrib otis.${tag}.grib -i -o salt.$level -nh > /dev/null
      ./hotis salt.$level salt $level
     else
      echo Should have found 1 field for level $level Salinities, found $y
      echo cannot retrieve this for later use.  Create a dummy level.
    fi
  
done
# At this point have all of the layers converted.  Now it is time to
#   assemble these in to the cofs vertical.
./votis asdf
cp tsout /data/new/otis.cofs/tsout.$tag

